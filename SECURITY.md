# Security Policy

## Supported Versions

Currently supporting security updates for:

| Version | Supported          |
| ------- | ------------------ |
| 0.5.x   | :white_check_mark: |
| < 0.5.0 | :x:                |

## Reporting a Vulnerability

We take security seriously. If you discover a security vulnerability, please follow these steps:

### 1. Do NOT Create a Public Issue

Security vulnerabilities should not be reported via public GitHub issues.

### 2. Report Privately

Send details to: security@[project-domain] (or via GitHub Security Advisories)

Include:
- Description of the vulnerability
- Steps to reproduce
- Potential impact
- Suggested fix (if any)

### 3. Response Timeline

- **Initial Response**: Within 48 hours
- **Status Update**: Within 7 days
- **Resolution Target**: Within 30 days for critical issues

## Security Considerations

### API Token Security

**GitHub Token (`GITHUB_TOKEN`)**
- Never commit tokens to repository
- Use environment variables only
- Minimum required permissions:
  - `repo`: For updating topics
  - `read:org`: For organization repos

**Best Practices**:
```bash
# Good - environment variable
export GITHUB_TOKEN="ghp_xxxxxxxxxxxx"
ollama-topic-forge

# Bad - command line argument (visible in process list)
ollama-topic-forge --token="ghp_xxxxxxxxxxxx"  # DON'T DO THIS
```

### Ollama Server Security

1. **Local Only by Default**
   - Ollama binds to localhost (127.0.0.1:11434)
   - Do not expose to network without authentication

2. **Network Exposure** (if needed)
   ```bash
   # Secure with reverse proxy and authentication
   # Never expose Ollama directly to internet
   ```

### Input Validation

The tool validates:
- Repository names (prevent injection)
- Topic format (alphanumeric + hyphens only)
- JSON responses (structured parsing)
- File paths (prevent traversal)

### Dependencies

Regular dependency updates:
```bash
# Check for outdated packages
npm audit  # For JS dependencies
cargo audit  # For Rust dependencies
pip-audit  # For Python dependencies
```

## Security Features

### Built-in Protections

1. **Input Sanitization**
   - All user inputs validated
   - Regex patterns for topics
   - Path traversal prevention

2. **API Rate Limiting**
   - Exponential backoff
   - Maximum retry limits
   - Prevents DoS on APIs

3. **Error Handling**
   - No sensitive data in errors
   - Generic messages to users
   - Detailed logs locally only

4. **Dependency Verification**
   - Script checksums validated
   - TLS for all API calls
   - Certificate pinning (future)

### Configuration Security

```bash
# Secure configuration
chmod 600 ~/.config/ollama-topic-forge/config
chmod 700 ~/.config/ollama-topic-forge/
```

## Known Security Considerations

1. **LLM Prompt Injection**
   - Repository data could contain malicious prompts
   - Mitigation: Structured format parameter limits LLM freedom

2. **JSON Parsing**
   - Malformed JSON from Ollama could cause issues
   - Mitigation: Using `jq` for robust parsing

3. **Shell Command Injection**
   - User input never directly used in shell commands
   - All parameters properly quoted

## Security Checklist for Contributors

- [ ] No hardcoded credentials
- [ ] All inputs validated
- [ ] Error messages don't leak sensitive info
- [ ] Dependencies up to date
- [ ] Shell commands properly escaped
- [ ] File operations use absolute paths
- [ ] API tokens from environment only

## Incident Response

If a security issue is discovered:

1. **Assess Impact**: Determine affected versions
2. **Develop Fix**: Create patch on private branch
3. **Test Thoroughly**: Ensure fix doesn't break functionality
4. **Release Update**: Push fix with security advisory
5. **Notify Users**: Via GitHub Security Advisory

## Future Security Enhancements

- [ ] Add GPG signature verification
- [ ] Implement API token encryption at rest
- [ ] Add audit logging
- [ ] Support for security scanning in CI/CD
- [ ] SBOM (Software Bill of Materials) generation

## Contact

For security concerns: security@[project-domain]

For general issues: Use GitHub Issues

---

*This security policy is adapted from standard open source security practices.*