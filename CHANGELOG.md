# Changelog

## 0.5.0 - 2026-07-18

### Added

- Optional Fluxion web administration system with web-enabled and headless standalone builds.
- Separate `:sasl-account` network setting, including configuration serialization and web administration support.
- Explicit downstream nickname synchronization when clients attach to an existing upstream connection.
- Release regression coverage for shared nickname handling, valued IRCv3 capabilities, SASL identities, and credential redaction.

### Changed

- Synthetic downstream hostmasks now use the configured upstream IRC username instead of the CLoak account name.
- Downstream nickname changes are restricted to reclaiming the configured primary upstream nickname, preventing one client from unexpectedly renaming a shared connection.
- SASL PLAIN now uses a standards-compliant authorization/authentication payload and falls back to the configured nick when `:sasl-account` is absent.
- IRCv3 capability matching now recognizes capabilities with values, including Libera.Chat's `sasl=...` advertisement.
- `make test` now loads test dependencies through Quicklisp, runs the suite once, and returns the correct status.

### Security

- Server `PASS` values and encoded SASL authentication payloads are redacted from logs.

### Compatibility

- Existing network configurations remain valid. Adding `:sasl-account` is optional; it defaults to the configured nick.
