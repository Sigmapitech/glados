---
title: Cabal & Hackage Security History
description: A timeline of security breaches and vulnerabilities in the Cabal ecosystem (Dec 2025 - Jan 2026).
sidebar:
  label: Security Audit
  order: 4
---

## Timeline (Dec 2025 - Jan 2026)

This log tracks security incidents affecting the `cabal-install` tool and the `Hackage` package repository.

### December 2025

| Period | Status | Details |
| :--- | :--- | :--- |
| **Week 1 (Dec 1 – 7)** | <span style="color:green">●</span> Safe | None. |
| **Week 2 (Dec 8 – 14)** | <span style="color:green">●</span> Safe | None. |
| **Week 3 (Dec 15 – 21)** | <span style="color:green">●</span> Safe | None. |
| **Week 4 (Dec 22 – 28)** | <span style="color:green">●</span> Safe | None. |

### January 2026

| Period | Status | Details |
| :--- | :--- | :--- |
| **Week 1 (Dec 29 – Jan 4)** | <span style="color:green">●</span> Safe | None. |
| **Week 2 (Jan 5 – Jan 11)** | <span style="color:green">●</span> Safe | None. |
| **Week 3 (Jan 12 – Jan 18)** | <span style="color:red">●</span> **Critical** | **Jan 16: Infrastructure Breach (HSEC-2024-0004)** |

---

## Incident Report: Jan 16, 2026

### Critical Infrastructure Breach (HSEC-2024-0004)

* **Target:** `hackage-server` and `hackage.haskell.org`
* **Vulnerability:** Stored Cross-Site Scripting (XSS)
* **Impact:**
  * Malicious HTML/JS files could be served via source packages or documentation uploads.
  * This exposed users to potential **session hijacking** when viewing compromised package pages.
* **Resolution:**
  * The Haskell Security Response Team (SRT) publicly disclosed the issue.
  * **Mitigation:** User content was migrated to a sandboxed domain (`hackage-content.haskell.org`) to prevent script execution on the main domain.

:::note[Action Required]
Ensure your local cabal config does not bypass security checks for package downloads. No update to the `cabal` binary itself is required for this server-side fix.
:::
