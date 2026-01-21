---
title: Documentation System Comparison
description: A comparative study between Astro, Sphinx, GitBook, and Pure Markdown, and the rationale behind our choice.
sidebar:
  label: Tech Stack Comparison
  order: 1
---

## Comparative Study

| Feature | **Astro (Starlight)** | **Sphinx** | **GitBook** | **Pure Markdown** |
| :--- | :--- | :--- | :--- | :--- |
| **Type** | Modern Static Site Generator (SSG) | Python-based Generator | Proprietary SaaS Platform | Raw Text Files |
| **Input Format** | MDX (Markdown + Components) | reStructuredText (reST) / MD | WYSIWYG + Markdown Sync | Markdown (.md) |
| **Performance** | **Best.** Zero-JS default (Islands). | Good, heavier HTML. | Moderate. Heavy scripts. | N/A (Viewer dependent). |
| **Customization** | **High.** React, Vue, Tailwind. | Moderate. Requires Python. | **Low.** Locked ecosystem. | **None.** |
| **Dev Experience** | **Excellent.** JS/TS stack. | Good for Python devs. | Low (GUI-focused). | Basic. |
| **Pricing** | Free (Open Source). | Free (Open Source). | Freemium (Expensive teams). | Free. |

### 1. Pure Markdown

- **Pros:** Zero setup, universally readable, lives in repo.
- **Cons:** No navigation, search, branding, or interactivity. It is a file format, not a system.

### 2. Sphinx

- **Pros:** Industry standard for Python. Great at API docs and cross-referencing.
- **Cons:** Relies on `reStructuredText`. Theming is often dated. Extending functionality requires Python knowledge.

### 3. GitBook

- **Pros:** "Just works." Great for non-technical writers.
- **Cons:** **Vendor Lock-in.** Limited customization. Expensive for private team docs. Heavy client-side JS.

### 4. Astro (Starlight)

- **Pros:** Best-in-class performance. Supports interactive components (MDX). "Docs-as-code" workflow.
- **Cons:** Requires web dev knowledge (Node.js) to set up.

---

## Why We Chose Astro

We selected Astro (specifically the Starlight template) for the best balance of **developer control** and **performance**.

### 1. "Docs-as-Code" Philosophy

Unlike GitBook, Astro lives in our repository. Documentation changes are reviewed in Pull Requests alongside code, fitting our existing CI/CD workflow.

### 2. MDX & Component Islands

We are not limited to static text.

- **Scenario:** To document a UI component, we import the *actual* component into the docs using MDX.
- Astro renders static HTML by default but hydrates it with JavaScript only when necessary (Islands Architecture).

### 3. No Vendor Lock-in

We own the pipeline. Hosting on Vercel, Netlify, or a private VPS is free and under our control.

### 4. Modern Tech Stack

It utilizes modern TypeScript and Tailwind CSS, making styling and maintenance easier for developers familiar with current web standards.

> **Conclusion:** We chose Astro because it provides the structure of Sphinx and the modernity of a Single Page App without the cost or lock-in of GitBook.
