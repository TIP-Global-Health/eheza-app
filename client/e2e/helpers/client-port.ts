import { readFileSync } from 'fs';
import { resolve } from 'path';

/**
 * Read the host port from .ddev/docker-compose.client-http.yaml.
 * Falls back to 3000 if the file is missing or unparseable.
 */
export function getClientPort(): number {
  try {
    const yamlPath = resolve(__dirname, '../../../.ddev/docker-compose.client-http.yaml');
    const content = readFileSync(yamlPath, 'utf-8');
    const match = content.match(/- (\d+):3000/);
    if (match) return parseInt(match[1], 10);
  } catch {
    // File not found or unreadable — use default.
  }
  return 3000;
}
