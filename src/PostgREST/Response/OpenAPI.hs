{-|
Module      : PostgREST.OpenAPI
Description : Generates the OpenAPI output
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.Response.OpenAPI (encode) where

import           Data.Aeson            ((.=))
import qualified Data.Aeson            as JSON
import qualified Data.Aeson.Key        as K
import qualified Data.Aeson.KeyMap     as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet.InsOrd   as Set
import qualified Data.Text             as T

import Control.Arrow              ((&&&))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap, fromList)
import Data.Maybe                 (fromJust)
import Data.String                (IsString (..))
import Network.URI                (URI (..), URIAuth (..))

import Control.Lens (at, (.~), (?~))

import Data.Swagger

import PostgREST.Config                   (AppConfig (..), Proxy (..),
                                           isMalformedProxyUri, toURI)
import PostgREST.MediaType
import PostgREST.Network                  (escapeHostName)
import PostgREST.SchemaCache              (CompositeType (..),
                                           ComputedField (..),
                                           SchemaCache (..))
import PostgREST.SchemaCache.Identifiers  (QualifiedIdentifier (..))
import PostgREST.SchemaCache.Relationship (Cardinality (..),
                                           Relationship (..),
                                           RelationshipsMap)
import PostgREST.SchemaCache.Routine      (FuncVolatility (..),
                                           PgType (..), RetType (..),
                                           Routine (..),
                                           RoutineParam (..))
import PostgREST.SchemaCache.Table        (Column (..), Table (..),
                                           TablesMap,
                                           tableColumnsList)

import Protolude hiding (Proxy, get)

encode :: (Text, Text) -> AppConfig -> SchemaCache -> TablesMap -> HM.HashMap k [Routine] -> Maybe Text -> LBS.ByteString
encode versions conf sCache tables procs schemaDescription =
  JSON.encode withMetadata
  where
    tablesList = snd <$> HM.toList tables
    routines   = concat $ HM.elems procs
    spec = JSON.toJSON $
      postgrestSpec
        versions
        (dbRelationships sCache)
        routines
        tablesList
        (proxyUri conf)
        schemaDescription
        (configOpenApiSecurityActive conf)
    -- Opt-in: inject the typegen metadata block as a top-level vendor extension.
    -- swagger2 has no typed extensions field, so we post-process the JSON Object.
    withMetadata
      | configOpenApiMetadata conf = case spec of
          JSON.Object o ->
            JSON.Object $
              KM.insert (K.fromText "x-postgrest-typegen-metadata")
                (typegenMetadata (dbRelationships sCache) routines tablesList
                  (dbCompositeTypes sCache) (dbComputedFields sCache))
                o
          other -> other
      | otherwise = spec

makeMimeList :: [MediaType] -> MimeList
makeMimeList cs = MimeList $ fmap (fromString . BS.unpack . toMime) cs

toSwaggerType :: Text -> Maybe (SwaggerType t)
toSwaggerType "character varying" = Just SwaggerString
toSwaggerType "character"         = Just SwaggerString
toSwaggerType "text"              = Just SwaggerString
toSwaggerType "boolean"           = Just SwaggerBoolean
toSwaggerType "smallint"          = Just SwaggerInteger
toSwaggerType "integer"           = Just SwaggerInteger
toSwaggerType "bigint"            = Just SwaggerInteger
toSwaggerType "numeric"           = Just SwaggerNumber
toSwaggerType "real"              = Just SwaggerNumber
toSwaggerType "double precision"  = Just SwaggerNumber
toSwaggerType "json"              = Nothing
toSwaggerType "jsonb"             = Nothing
toSwaggerType colType             = case T.takeEnd 2 colType of
  "[]" -> Just SwaggerArray
  _    -> Just SwaggerString

toSwaggerFormat :: Text -> Maybe Text
toSwaggerFormat "smallint" = Just "int32"
toSwaggerFormat "integer"  = Just "int32"
toSwaggerFormat "bigint"   = Just "int64"
toSwaggerFormat colType    = Just colType

typeFromArray :: Text -> Text
typeFromArray = T.dropEnd 2

toSwaggerTypeFromArray :: Text -> Maybe (SwaggerType t)
toSwaggerTypeFromArray arrType = toSwaggerType $ typeFromArray arrType

makePropertyItems :: Text -> Maybe (Referenced Schema)
makePropertyItems arrType = case toSwaggerType arrType of
  Just SwaggerArray -> Just $ Inline (mempty & type_ .~ toSwaggerTypeFromArray arrType)
  _                 -> Nothing

parseDefault :: Text -> Text -> Text
parseDefault colType colDefault =
  case toSwaggerType colType of
    Just SwaggerString -> wrapInQuotations $ case T.stripSuffix ("::" <> colType) colDefault of
      Just def -> T.dropAround (=='\'')  def
      Nothing  -> colDefault
    _ -> colDefault
  where
    wrapInQuotations text = "\"" <> text <> "\""

makeTableDef :: RelationshipsMap -> Table -> (Text, Schema)
makeTableDef rels t =
  let tn = tableName t in
      (tn, (mempty :: Schema)
        & description .~ tableDescription t
        & type_ ?~ SwaggerObject
        & properties .~ fromList (makeProperty t rels <$> tableColumnsList t)
        & required .~ fmap colName (filter (not . colNullable) $ tableColumnsList t))

makeProperty :: Table -> RelationshipsMap -> Column -> (Text, Referenced Schema)
makeProperty tbl rels col = (colName col, Inline s)
  where
    e = if null $ colEnum col then Nothing else JSON.decode $ JSON.encode $ colEnum col
    fk :: Maybe Text
    fk =
      let
        searchedRels = fromMaybe mempty $ HM.lookup (QualifiedIdentifier (tableSchema tbl) (tableName tbl), tableSchema tbl) rels
        -- Sorts the relationship list to get tables first
        relsSortedByIsView = sortOn relFTableIsView [ r | r@Relationship{} <- searchedRels]
        -- Finds the relationship that has a single column foreign key
        rel = find (\case
          Relationship{relCardinality=(M2O _ relColumns)}       -> [colName col] == (fst <$> relColumns)
          Relationship{relCardinality=(O2O _ relColumns False)} -> [colName col] == (fst <$> relColumns)
          _                                                     -> False
          ) relsSortedByIsView
        fCol = (headMay . (\r -> snd <$> relColumns (relCardinality r)) =<< rel)
        fTbl = qiName . relForeignTable <$> rel
        fTblCol = (,) <$> fTbl <*> fCol
      in
        (\(a, b) -> T.intercalate "" ["This is a Foreign Key to `", a, ".", b, "`.<fk table='", a, "' column='", b, "'/>"]) <$> fTblCol
    pk :: Bool
    pk = colName col `elem` tablePKCols tbl
    n = catMaybes
      [ Just "Note:"
      , if pk then Just "This is a Primary Key.<pk/>" else Nothing
      , fk
      ]
    d =
      if length n > 1 then
        Just $ T.append (maybe "" (`T.append` "\n\n") $ colDescription col) (T.intercalate "\n" n)
      else
        colDescription col
    s =
      (mempty :: Schema)
        & default_ .~ (JSON.decode . toUtf8Lazy . parseDefault (colType col) =<< colDefault col)
        & description .~ d
        & enum_ .~ e
        & format .~ toSwaggerFormat (colType col)
        & maxLength .~ (fromIntegral <$> colMaxLen col)
        & type_ .~ toSwaggerType (colType col)
        & items .~ (SwaggerItemsObject <$> makePropertyItems (colType col))

makeProcSchema :: Routine -> Schema
makeProcSchema pd =
  (mempty :: Schema)
  & description .~ pdDescription pd
  & type_ ?~ SwaggerObject
  & properties .~ fromList (fmap makeProcProperty (pdParams pd))
  & required .~ fmap ppName (filter ppReq (pdParams pd))

makeProcProperty :: RoutineParam -> (Text, Referenced Schema)
makeProcProperty (RoutineParam n t _ _ _) = (n, Inline s)
  where
    s = (mempty :: Schema)
          & type_ .~ toSwaggerType t
          & items .~ (SwaggerItemsObject <$> makePropertyItems t)
          & format .~ toSwaggerFormat t

makePreferParam :: [Text] -> Param
makePreferParam ts =
  (mempty :: Param)
  & name        .~ "Prefer"
  & description ?~ "Preference"
  & required    ?~ False
  & schema .~ ParamOther ((mempty :: ParamOtherSchema)
    & in_ .~ ParamHeader
    & type_ ?~ SwaggerString
    & enum_ .~ if null enu then Nothing else JSON.decode (JSON.encode enu))
  where
    enu = foldl (<>) [] (val <$> ts)
    val :: Text -> [Text]
    val = \case
      "count"      -> ["count=none"]
      "return"     -> ["return=representation", "return=minimal", "return=none"]
      "resolution" -> ["resolution=ignore-duplicates", "resolution=merge-duplicates"]
      _            -> []

makeProcGetParam :: RoutineParam -> Referenced Param
makeProcGetParam (RoutineParam n t _ r v) =
  Inline $ (mempty :: Param)
    & name .~ n
    & required ?~ r
    & schema .~ ParamOther fullSchema
  where
    fullSchema = if v then schemaMulti else schemaNotMulti
    baseSchema = (mempty :: ParamOtherSchema)
      & in_ .~ ParamQuery
    schemaNotMulti = baseSchema
      & format .~ toSwaggerFormat t
      & type_ ?~ toParamType (toSwaggerType t)
    schemaMulti = baseSchema
      & type_ ?~ fromMaybe SwaggerString (toSwaggerType t)
      & items ?~ SwaggerItemsPrimitive (Just CollectionMulti)
        ((mempty :: ParamSchema x)
          & type_ .~ toSwaggerTypeFromArray t
          & format .~ toSwaggerFormat (typeFromArray t))
    toParamType paramType = case paramType of
      -- Array uses {} in query params
      Just SwaggerArray -> SwaggerString
      -- Type must be specified in query params
      Nothing           -> SwaggerString
      _                 -> fromJust paramType

makeProcGetParams :: [RoutineParam] -> [Referenced Param]
makeProcGetParams = fmap makeProcGetParam

makeProcPostParams :: Routine -> [Referenced Param]
makeProcPostParams pd =
  [ Inline $ (mempty :: Param)
    & name     .~ "args"
    & required ?~ True
    & schema   .~ ParamBody (Inline $ makeProcSchema pd)
  , Ref $ Reference "preferParams"
  ]

makeParamDefs :: [Table] -> [(Text, Param)]
makeParamDefs ti =
  -- TODO: create Prefer for each method (GET, PATCH, etc.)
  [ ("preferParams", makePreferParam ["params"])
  , ("preferReturn", makePreferParam ["return"])
  , ("preferCount", makePreferParam ["count"])
  , ("preferPost", makePreferParam ["return", "resolution"])
  , ("select", (mempty :: Param)
      & name        .~ "select"
      & description ?~ "Filtering Columns"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamQuery
        & type_ ?~ SwaggerString))
  , ("on_conflict", (mempty :: Param)
      & name        .~ "on_conflict"
      & description ?~ "On Conflict"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamQuery
        & type_ ?~ SwaggerString))
  , ("order", (mempty :: Param)
      & name        .~ "order"
      & description ?~ "Ordering"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamQuery
        & type_ ?~ SwaggerString))
  , ("range", (mempty :: Param)
      & name        .~ "Range"
      & description ?~ "Limiting and Pagination"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamHeader
        & type_ ?~ SwaggerString))
  , ("rangeUnit", (mempty :: Param)
      & name        .~ "Range-Unit"
      & description ?~ "Limiting and Pagination"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamHeader
        & type_ ?~ SwaggerString
        & default_ .~ JSON.decode "\"items\""))
  , ("offset", (mempty :: Param)
      & name        .~ "offset"
      & description ?~ "Limiting and Pagination"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamQuery
        & type_ ?~ SwaggerString))
  , ("limit", (mempty :: Param)
      & name        .~ "limit"
      & description ?~ "Limiting and Pagination"
      & required    ?~ False
      & schema .~ ParamOther ((mempty :: ParamOtherSchema)
        & in_ .~ ParamQuery
        & type_ ?~ SwaggerString))
  ]
  <> concat [ makeObjectBody (tableName t) : makeRowFilters (tableName t) (tableColumnsList t)
            | t <- ti
            ]

makeObjectBody :: Text -> (Text, Param)
makeObjectBody tn =
  ("body." <> tn, (mempty :: Param)
     & name .~ tn
     & description ?~ tn
     & required ?~ False
     & schema .~ ParamBody (Ref (Reference tn)))

makeRowFilter :: Text -> Column -> (Text, Param)
makeRowFilter tn c =
  (T.intercalate "." ["rowFilter", tn, colName c], (mempty :: Param)
    & name .~ colName c
    & description .~ colDescription c
    & required ?~ False
    & schema .~ ParamOther ((mempty :: ParamOtherSchema)
      & in_ .~ ParamQuery
      & type_ ?~ SwaggerString))

makeRowFilters :: Text -> [Column] -> [(Text, Param)]
makeRowFilters tn = fmap (makeRowFilter tn)

makePathItem :: Table -> (FilePath, PathItem)
makePathItem t = ("/" ++ T.unpack tn, p $ tableInsertable t || tableUpdatable t || tableDeletable t)
  where
    -- Use first line of table description as summary; rest as description (if present)
    -- We strip leading newlines from description so that users can include a blank line between summary and description
    (tSum, tDesc) = fmap fst &&& fmap (T.dropWhile (=='\n') . snd) $
                    T.breakOn "\n" <$> tableDescription t
    tOp = (mempty :: Operation)
      & tags .~ Set.fromList [tn]
      & summary .~ tSum
      & description .~ mfilter (/="") tDesc
    getOp = tOp
      & parameters .~ fmap ref (rs <> ["select", "order", "range", "rangeUnit", "offset", "limit", "preferCount"])
      & at 206 ?~ "Partial Content"
      & at 200 ?~ Inline ((mempty :: Response)
        & description .~ "OK"
        & schema ?~ Inline (mempty
          & type_ ?~ SwaggerArray
          & items ?~ SwaggerItemsObject (Ref $ Reference $ tableName t)
        )
      )
    postOp = tOp
      & parameters .~ fmap ref ["body." <> tn, "select", "preferPost"]
      & at 201 ?~ "Created"
    patchOp = tOp
      & parameters .~ fmap ref (rs <> ["body." <> tn, "preferReturn"])
      & at 204 ?~ "No Content"
    deletOp = tOp
      & parameters .~ fmap ref (rs <> ["preferReturn"])
      & at 204 ?~ "No Content"
    pr = (mempty :: PathItem) & get ?~ getOp
    pw = pr & post ?~ postOp & patch ?~ patchOp & delete ?~ deletOp
    p False = pr
    p True  = pw
    tn = tableName t
    rs = [ T.intercalate "." ["rowFilter", tn, colName c ] | c <- tableColumnsList t ]
    ref = Ref . Reference

makeProcPathItem :: Routine -> (FilePath, PathItem)
makeProcPathItem pd = ("/rpc/" ++ toS (pdName pd), pe)
  where
    -- Use first line of proc description as summary; rest as description (if present)
    -- We strip leading newlines from description so that users can include a blank line between summary and description
    (pSum, pDesc) = fmap fst &&& fmap (T.dropWhile (=='\n') . snd) $
                    T.breakOn "\n" <$> pdDescription pd
    procOp = (mempty :: Operation)
      & summary .~ pSum
      & description .~ mfilter (/="") pDesc
      & tags .~ Set.fromList ["(rpc) " <> pdName pd]
      & produces ?~ makeMimeList [MTApplicationJSON, MTVndSingularJSON True, MTVndSingularJSON False]
      & at 200 ?~ "OK"
    getOp = procOp
      & parameters .~ makeProcGetParams (pdParams pd)
    postOp = procOp
      & parameters .~ makeProcPostParams pd
    pe = case pdVolatility pd of
      Volatile -> (mempty :: PathItem) & post ?~ postOp
      _        -> (mempty :: PathItem) & get ?~ getOp & post ?~ postOp

makeRootPathItem :: (FilePath, PathItem)
makeRootPathItem = ("/", p)
  where
    getOp = (mempty :: Operation)
      & tags .~ Set.fromList ["Introspection"]
      & summary ?~ "OpenAPI description (this document)"
      & produces ?~ makeMimeList [MTOpenAPI, MTApplicationJSON]
      & at 200 ?~ "OK"
    pr = (mempty :: PathItem) & get ?~ getOp
    p = pr

makePathItems :: [Routine] -> [Table] -> InsOrdHashMap FilePath PathItem
makePathItems pds ti = fromList $ makeRootPathItem :
  fmap makePathItem ti ++ fmap makeProcPathItem pds

makeSecurityDefinitions :: Text -> Bool -> SecurityDefinitions
makeSecurityDefinitions secName allow
  | allow = SecurityDefinitions (fromList [(secName, SecurityScheme secSchType secSchDescription)])
  | otherwise    = mempty
  where
    secSchType = SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader)
    secSchDescription = Just "Add the token prepending \"Bearer \" (without quotes) to it"

postgrestSpec :: (Text, Text) -> RelationshipsMap -> [Routine] -> [Table] -> (Text, Text, Integer, Text) -> Maybe Text -> Bool -> Swagger
postgrestSpec (prettyVersion, docsVersion) rels pds ti (s, h, p, b) sd allowSecurityDef = (mempty :: Swagger)
  & basePath ?~ T.unpack b
  & schemes ?~ [s']
  & info .~ ((mempty :: Info)
      & version .~ prettyVersion
      & title .~ fromMaybe "PostgREST API" dTitle
      & description ?~ fromMaybe "This is a dynamic API generated by PostgREST" dDesc)
  & externalDocs ?~ ((mempty :: ExternalDocs)
    & description ?~ "PostgREST Documentation"
    & url .~ URL ("https://postgrest.org/en/" <> docsVersion <> "/references/api.html"))
  & host .~ h'
  & definitions .~ fromList (makeTableDef rels <$> ti)
  & parameters .~ fromList (makeParamDefs ti)
  & paths .~ makePathItems pds ti
  & produces .~ makeMimeList [MTApplicationJSON, MTVndSingularJSON True, MTVndSingularJSON False, MTTextCSV]
  & consumes .~ makeMimeList [MTApplicationJSON, MTVndSingularJSON True, MTVndSingularJSON False, MTTextCSV]
  & securityDefinitions .~ makeSecurityDefinitions securityDefName allowSecurityDef
  & security .~ [SecurityRequirement (fromList [(securityDefName, [])]) | allowSecurityDef]
    where
      s' = if s == "http" then Http else Https
      h' = Just $ Host (T.unpack $ escapeHostName h) (Just (fromInteger p))
      securityDefName = "JWT"
      (dTitle, dDesc) = fmap fst &&& fmap (T.dropWhile (=='\n') . snd) $
                    T.breakOn "\n" <$> sd

pickProxy :: Maybe Text -> Maybe Proxy
pickProxy proxy
  | isNothing proxy = Nothing
  -- should never happen
  -- since the request would have been rejected by the middleware if proxy uri
  -- is malformed
  | isMalformedProxyUri $ fromMaybe mempty proxy = Nothing
  | otherwise = Just Proxy {
    proxyScheme = scheme
  , proxyHost = host'
  , proxyPort = port''
  , proxyPath = path'
  }
 where
   uri = toURI $ fromJust proxy
   scheme = T.init $ T.toLower $ T.pack $ uriScheme uri
   path URI {uriPath = ""} =  "/"
   path URI {uriPath = p}  = p
   path' = T.pack $ path uri
   authority = fromJust $ uriAuthority uri
   host' = T.pack $ uriRegName authority
   port' = uriPort authority
   readPort = fromMaybe 80 . readMaybe
   port'' :: Integer
   port'' = case (port', scheme) of
             ("", "http")  -> 80
             ("", "https") -> 443
             _             -> readPort $ T.unpack $ T.tail $ T.pack port'

proxyUri :: AppConfig -> (Text, Text, Integer, Text)
proxyUri AppConfig{..} =
  case pickProxy $ toS <$> configOpenApiServerProxyUri of
    Just Proxy{..} ->
      (proxyScheme, proxyHost, proxyPort, proxyPath)
    Nothing ->
      ("http", configServerHost, toInteger configServerPort, "/")

-- | Build the `x-postgrest-typegen-metadata` block: a name-keyed projection of
-- the schema cache mirroring `@supabase/postgrest-typegen`'s GeneratorMetadata,
-- so types can be generated from the OpenAPI document alone. Opt-in via
-- `openapi-metadata`. Type names are emitted as pg_catalog short names
-- (int8, _text, user_status) to match the SQL introspection path.
typegenMetadata :: RelationshipsMap -> [Routine] -> [Table] -> [CompositeType] -> [ComputedField] -> JSON.Value
typegenMetadata relsMap routines tbls comps cfields = JSON.object
  [ "schemas"       .= [ JSON.object ["name" .= s] | s <- nubOn identity (map tableSchema tbls) ]
  , "tables"        .= map tableJ tbls
  , "relationships" .= concatMap relJ allRels
  , "functions"     .= map snd (nubOn fst funcEntries)
  , "composites"    .= map compJ comps
  , "enums"         .= map enumJ enumsList
  ]
  where
    -- The RelationshipsMap indexes each relationship under multiple keys, so
    -- flattening yields duplicates — dedupe before emitting.
    allRels = nubOn relKey (concat (HM.elems relsMap))
    relKey r = case r of
      Relationship{relTable, relForeignTable, relCardinality} ->
        T.intercalate "|"
          [ "rel", qiSchema relTable, qiName relTable
          , qiSchema relForeignTable, qiName relForeignTable, cardKey relCardinality ]
      ComputedRelationship{relFunction, relTable, relForeignTable} ->
        T.intercalate "|"
          [ "crel", qiSchema relFunction, qiName relFunction
          , qiName relTable, qiName relForeignTable ]
    colsKey cols = T.intercalate "," (map fst cols) <> ">" <> T.intercalate "," (map snd cols)
    cardKey c = case c of
      M2O cons cols   -> "m2o|" <> cons <> "|" <> colsKey cols
      O2O cons cols _ -> "o2o|" <> cons <> "|" <> colsKey cols
      O2M cons cols   -> "o2m|" <> cons <> "|" <> colsKey cols
      M2M _           -> "m2m"

    enumsList = nubOn (\(_, n, _) -> n)
      [ (tableSchema t, colFormat c, colEnum c)
      | t <- tbls, c <- tableColumnsList t, not (null (colEnum c)) ]
    enumJ (s, n, vs) = JSON.object ["schema" .= s, "name" .= n, "values" .= vs]

    tableJ t = JSON.object
      [ "schema"    .= tableSchema t
      , "name"      .= tableName t
      , "kind"      .= tableKind t
      , "updatable" .= tableUpdatable t -- drives view Insert/Update generation
      , "columns"   .= map (colJ (tableUpdatable t)) (tableColumnsList t)
      ]

    -- Per-column updatability isn't tracked by PostgREST; fall back to the
    -- relation-level flag (correct for tables and non-updatable views).
    colJ upd c = JSON.object
      [ "name"                .= colName c
      , "format"              .= colFormat c
      , "data_type"           .= colType c
      , "is_nullable"         .= colNullable c
      , "default_value"       .= colDefault c
      , "is_identity"         .= isJust (colIdentityGeneration c)
      , "identity_generation" .= colIdentityGeneration c
      , "is_generated"        .= colIsGenerated c
      , "is_updatable"        .= upd
      , "enums"               .= colEnum c
      , "comment"             .= colDescription c
      ]

    relJ rel = case rel of
      Relationship{relTable, relForeignTable, relCardinality} ->
        case relCardinality of
          M2O cons cols          -> [mkRel relTable relForeignTable cons cols False]
          -- Only the FK-owner side (isParent = False, per decodeRels). The
          -- inverse O2O (isParent = True), the O2M inverse, and M2M are not
          -- emitted as relationships by introspect().
          O2O cons cols isParent -> [mkRel relTable relForeignTable cons cols True | not isParent]
          O2M _ _                -> []
          M2M _                  -> []
      ComputedRelationship{} -> []

    mkRel src tgt cons cols oneToOne = JSON.object
      [ "constraint_name"     .= cons
      , "schema"              .= qiSchema src
      , "relation"            .= qiName src
      , "columns"             .= map fst cols
      , "is_one_to_one"       .= oneToOne
      , "referenced_schema"   .= qiSchema tgt
      , "referenced_relation" .= qiName tgt
      , "referenced_columns"  .= map snd cols
      ]

    routineJ pd = JSON.object
      [ "schema"         .= pdSchema pd
      , "name"           .= pdName pd
      , "argument_types" .= routineArgTypes pd
        -- input args + OUT/TABLE return columns (mode "table") so the generator
        -- can render `RETURNS TABLE(...)` columns instead of falling back to record.
      , "args"           .= (map argJ (pdParams pd) ++ map tableColArg (pdReturnColumns pd))
      , "return"         .= retJ (pdReturnType pd) (pdRows pd)
      , "volatility"     .= volJ (pdVolatility pd)
      ]
    tableColArg (cname, ctype) = JSON.object
      [ "name" .= cname, "type" .= ctype, "mode" .= ("table" :: Text), "has_default" .= False ]
    argJ p = JSON.object
      [ "name"        .= ppName p
      , "type"        .= shortType (ppType p)
      , "mode"        .= (if ppVar p then "variadic" else "in" :: Text)
      , "has_default" .= not (ppReq p)
      ]
    retJ rt rows = case rt of
      Single pgt -> retObj pgt False rows
      SetOf  pgt -> retObj pgt True rows
    retObj pgt isSet rows = JSON.object $
      [ "type"   .= pgTypeName pgt
      , "is_set" .= isSet
      , "rows"   .= rows
      ] ++ [ "relation" .= relationOf pgt ]
    relationOf pgt = case pgt of
      Composite qi _ -> JSON.object ["schema" .= qiSchema qi, "name" .= qiName qi]
      Scalar _       -> JSON.Null
    pgTypeName pgt = case pgt of
      Composite qi _ -> qiName qi
      Scalar qi      -> qiName qi

    cfieldJ cf = JSON.object
      [ "schema"         .= cfSchema cf
      , "name"           .= cfName cf
      , "argument_types" .= cfTableName cf
      , "args"           .= [ tableRowArg (cfTableName cf) ]
      , "return"         .= JSON.object
          [ "type"     .= cfReturnType cf
          , "is_set"   .= cfIsSetOf cf
          , "rows"     .= cfRows cf
          , "relation" .= if cfReturnIsRelation cf
              then JSON.object ["schema" .= cfReturnSchema cf, "name" .= cfReturnType cf]
              else JSON.Null
          ]
      , "volatility"     .= volJ (cfVolatility cf)
      ]

    -- All emitted functions, keyed by (schema, name, argument_types) and deduped
    -- so a single named table-row arg function (present in both dbRoutines and
    -- the computed-field/relationship sources) is emitted once. RPC routines win.
    funcEntries =
      [ ((pdSchema pd, pdName pd, routineArgTypes pd), routineJ pd) | pd <- routines ]
      ++ [ ((cfSchema cf, cfName cf, cfTableName cf), cfieldJ cf)
         | cf <- cfields
         , (cfSchema cf, cfName cf, cfTableName cf) `notElem` routineSingleArgSigs ]
    -- (schema, name, single-arg-type) of callable routines with exactly one arg:
    -- used to skip the computed-field/relationship copy of a function that is
    -- already a callable routine (introspect() lists it once, as the routine).
    routineSingleArgSigs =
      [ (pdSchema pd, pdName pd, shortType (ppType p))
      | pd <- routines, [p] <- [pdParams pd] ]
    -- Mirror introspect()'s argument_types: bare type for unnamed args, "name
    -- type" for named ones, so only unnamed single-table-arg functions match a
    -- table name and attach as computed fields.
    routineArgTypes pd = T.intercalate ", "
      [ if ppName p == "" then shortType (ppType p) else ppName p <> " " <> shortType (ppType p)
      | p <- pdParams pd ]
    tableRowArg tname = JSON.object
      [ "name" .= ("" :: Text), "type" .= tname, "mode" .= ("in" :: Text), "has_default" .= False ]

    compJ ct = JSON.object
      [ "schema"     .= ctSchema ct
      , "name"       .= ctName ct
      , "attributes" .= [ JSON.object ["name" .= n, "type" .= ty] | (n, ty) <- ctAttributes ct ]
      ]

    volJ v = case v of
      Immutable -> "IMMUTABLE" :: Text
      Stable    -> "STABLE"
      Volatile  -> "VOLATILE"

-- | Distinct-by-key, preserving first occurrence.
nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = go []
  where
    go _ [] = []
    go seen (x:xs)
      | f x `elem` seen = go seen xs
      | otherwise       = x : go (f x : seen) xs

-- | Map a pg regtype text (SQL spelling, possibly schema-qualified) to the
-- pg_catalog short type name the typegen generator expects (int8, _text, …).
shortType :: Text -> Text
shortType raw
  | "[]" `T.isSuffixOf` raw = "_" <> shortType (T.dropEnd 2 raw)
  | otherwise               = maybe bare snd (find ((== bare) . fst) sqlToShort)
  where
    bare = T.takeWhileEnd (/= '.') raw
    sqlToShort =
      [ ("integer", "int4"), ("bigint", "int8"), ("smallint", "int2")
      , ("boolean", "bool"), ("double precision", "float8"), ("real", "float4")
      , ("character varying", "varchar"), ("character", "bpchar")
      , ("timestamp with time zone", "timestamptz")
      , ("timestamp without time zone", "timestamp")
      , ("time with time zone", "timetz"), ("time without time zone", "time") ]
