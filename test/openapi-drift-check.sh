#!/usr/bin/env bash
# Generates or verifies a snapshot of the OpenAPI (Swagger 2.0) document
# emitted by PostgREST when run against the spec test fixture schema.
#
# Intended invocation (assumes nix devShell with withTools available):
#
#   # Regenerate the committed snapshot (write mode, default):
#   postgrest-with-pg-17 --fixtures test/spec/fixtures/load.sql \
#     postgrest-with-pgrst test/openapi-drift-check.sh
#
#   # Verify the committed snapshot is in sync (CI mode):
#   postgrest-with-pg-17 --fixtures test/spec/fixtures/load.sql \
#     postgrest-with-pgrst test/openapi-drift-check.sh --check
#
# In --check mode the script always writes the live spec to
# test/spec/fixtures/openapi-snapshot.generated.json so CI can upload it
# as an artifact even when the diff fails. That file is gitignored.
#
# The script expects PGRST_SERVER_UNIX_SOCKET to be set, which
# postgrest-with-pgrst provides.

set -euo pipefail

MODE="${1:-write}"
SNAPSHOT="test/spec/fixtures/openapi-snapshot.json"
GENERATED="test/spec/fixtures/openapi-snapshot.generated.json"

: "${PGRST_SERVER_UNIX_SOCKET:?PGRST_SERVER_UNIX_SOCKET is required; run this script via postgrest-with-pgrst}"

mkdir -p "$(dirname "$SNAPSHOT")"

curl -sSf -H "Accept: application/openapi+json" \
  --unix-socket "$PGRST_SERVER_UNIX_SOCKET" \
  http://localhost/ \
  | jq -S . > "$GENERATED"

case "$MODE" in
  --check)
    if [ ! -f "$SNAPSHOT" ]; then
      cat >&2 <<EOF
::error::Committed OpenAPI snapshot is missing.

The freshly generated spec has been written to:
  $GENERATED

If you have a CI run, download the "openapi-snapshot" artifact, inspect
it, and commit it as $SNAPSHOT.

Locally, you can run (inside nix develop):

  postgrest-with-pg-17 --fixtures test/spec/fixtures/load.sql \\
    postgrest-with-pgrst test/openapi-drift-check.sh

to produce the committed snapshot directly.
EOF
      exit 1
    fi
    if ! diff -u "$SNAPSHOT" "$GENERATED"; then
      cat >&2 <<EOF
::error::OpenAPI snapshot is out of date.

The freshly generated spec has been written to:
  $GENERATED

If this drift is intentional (you changed the spec generator or schema):
download the "openapi-snapshot" artifact from this CI run, replace
$SNAPSHOT with it, and commit.

If this drift is unintentional: fix the underlying code; do not commit
the regenerated snapshot.

Locally, you can regenerate with (inside nix develop):

  postgrest-with-pg-17 --fixtures test/spec/fixtures/load.sql \\
    postgrest-with-pgrst test/openapi-drift-check.sh
EOF
      exit 1
    fi
    echo "OpenAPI snapshot is up to date."
    ;;
  write|"")
    mv "$GENERATED" "$SNAPSHOT"
    echo "Wrote $SNAPSHOT"
    ;;
  *)
    echo "Unknown mode: $MODE (expected --check or write)" >&2
    exit 2
    ;;
esac