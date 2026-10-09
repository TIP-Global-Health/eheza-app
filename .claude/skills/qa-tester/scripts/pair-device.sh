#!/bin/bash
# Create a device node holding a pairing code, ready for a QA run to pair with.
#
#   bash .claude/skills/qa-tester/scripts/pair-device.sh [code]   # default 88888888
#
# Codes must be unique, so the code is first cleared off any device that still
# holds it. Super user mode is needed for that, and is put back as it was.

set -eu

code=${1:-88888888}
if ! [[ $code =~ ^[0-9]{8}$ ]]; then
  echo "usage: $0 [8-digit code]" >&2
  exit 1
fi
title="QA Manual Device $(date +%Y%m%d-%H%M%S)"

# ddev serves the main tree only, so run from there even when called from a worktree.
main=$(dirname "$(git -C "$(dirname "$0")" rev-parse --path-format=absolute --git-common-dir)")
cd "$main"

read -r -d '' php <<PHP || true
\$code = '$code';
\$prev = variable_get('hedley_super_user_mode', 0);
variable_set('hedley_super_user_mode', 1);
\$q = new EntityFieldQuery();
\$r = \$q->entityCondition('entity_type', 'node')
  ->propertyCondition('type', 'device')
  ->fieldCondition('field_pairing_code', 'value', \$code)
  ->execute();
if (!empty(\$r['node'])) {
  foreach (node_load_multiple(array_keys(\$r['node'])) as \$old) {
    \$old->field_pairing_code[LANGUAGE_NONE][0]['value'] = '';
    node_save(\$old);
  }
}
variable_set('hedley_super_user_mode', \$prev);
\$node = entity_create('node', array('type' => 'device', 'title' => '$title'));
\$w = entity_metadata_wrapper('node', \$node);
\$w->field_pairing_code->set(\$code);
node_save(\$node);
echo 'created: nid ' . \$node->nid . ', ' . '$title' . ', code ' . \$code . "\n";
PHP

ddev drush eval "$php"
