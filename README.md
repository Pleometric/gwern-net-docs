# gwern.net Codebase Documentation

Unofficial technical documentation for the [gwern.net](https://gwern.net) codebase.

**[View the Documentation](https://gwern.pleometric.net)**

---

## What is this?

This is a Docusaurus site containing 180+ documentation files covering the entire gwern.net codebase:

| Category | Files | Coverage |
|----------|-------|----------|
| Haskell (backend) | 72 | Build pipeline, annotation system, link processing |
| JavaScript (frontend) | 28 | Popup system, transclusion, event framework |
| PHP | 21 | Asset bundling, CSS generation |
| Python | 13 | LLM-based text processing |
| Shell | 11 | Archiving, embedding, utilities |
| CSS | 8 | Theming, colors, reader mode |
| Templates | 17 | Hakyll/Pandoc HTML templates |
| Nginx | 4 | Server configuration |

## Why does this exist?

[gwern.net](https://gwern.net) is a long-form essay site by [Gwern Branwen](https://gwern.net/about), known for rigorous research essays on AI, statistics, psychology, and more. What makes it technically interesting is its sophisticated custom infrastructure:

- **Hover popups** showing link previews and citations without leaving the page
- **Inline transclusion** embedding content from other pages
- **Margin sidenotes** keeping annotations visible alongside text
- **Automatic bibliography generation** from link metadata
- **Preemptive link archiving** mirroring external links locally to prevent link rot

The backend uses [Hakyll](https://jaspervdj.be/hakyll/) (Haskell) with [Pandoc](https://pandoc.org/) for Markdown processing. The frontend is a custom vanilla JavaScript framework built around a pub/sub event system. The codebase has been in active development for over a decade.

I started this project out of curiosity after reading Gwern's site for years. The [source code is on GitHub](https://github.com/gwern/gwern.net) but it's large and constantly evolving. These docs aim to make exploration easier and serve as a map for anyone (human or AI) wanting to understand how it all works.

## Source Repositories

- **Main site**: [github.com/gwern/gwern.net](https://github.com/gwern/gwern.net)
- **Said Achmiz** (frontend author): [github.com/achmizs](https://github.com/achmizs)

These docs are based on gwern.net commit [`57e5aa5df`](https://github.com/gwern/gwern.net/commit/57e5aa5df) (2025-02-01).

## How These Docs Were Created

1. **Initial mapping**: I read through the codebase and wrote notes on the architecture and key files

2. **Documentation generation**: Multiple [Claude Opus 4.5](https://www.anthropic.com/claude) agents documented different parts of the codebase in parallel, using my notes as a guide

3. **Manual review**: I reviewed and edited the generated documentation for accuracy and completeness

4. **Automated verification**: [OpenAI Codex 5.2](https://openai.com) performed claim verification, cross-referencing 92 specific technical claims against the actual source code

5. **Corrections applied**: All verified corrections were implemented and the overview documents updated

### Disclaimer

These docs are unofficial and not affiliated with Gwern Branwen or Said Achmiz. Every now and then I find a new mistake or not-quite-accurate description. If you find any problems, please open an issue or PR.

## Quick Start

### View Online

Visit the deployed site (link TBD after GitHub Pages setup).

### Run Locally

```bash
# Clone the repo
git clone https://github.com/YOUR_USERNAME/gwern-net-docs.git
cd gwern-net-docs

# Install dependencies
npm install

# Start dev server
npm run start
```

The site will be available at `http://localhost:3000`.

### Build for Production

```bash
npm run build
```

Static files will be generated in the `build/` directory.

## Documentation Structure

| Section | Description |
|---------|-------------|
| [Architecture at a Glance](docs/overview/architecture-at-a-glance.md) | High-level overview of build-time and runtime phases |
| [Page Lifecycle](docs/overview/page-lifecycle.md) | How a Markdown file becomes an interactive web page |
| [Functional Taxonomy](docs/overview/component-taxonomy.md) | Every file organized by function with importance scores |

### By Category

- **Backend** (`docs/backend/`): Haskell modules for build pipeline, annotations, link processing
- **Frontend** (`docs/frontend/`): JavaScript for popups, transclusion, theming, DOM transforms
- **PHP** (`docs/php/`): Asset bundling, CSS generation, SVG optimization
- **Python** (`docs/python/`): LLM-based text processing (paragraph splitting, title cleaning)
- **Shell** (`docs/shell/`): Embedding generation, link archiving, file uploads
- **CSS** (`docs/css/`): Color systems, dark mode, reader mode
- **Templates** (`docs/templates/`): Hakyll/Pandoc HTML templates
- **Nginx** (`docs/nginx/`): Server configuration and redirects

## Contributing

Found an error? Have improvements? PRs welcome.

1. Fork the repo
2. Make your changes
3. Submit a PR with a clear description

## License

Documentation content is provided as-is for educational purposes. The gwern.net codebase itself is maintained by Gwern Branwen and Said Achmiz under their own terms.

## See Also

- [gwern.net/about](https://gwern.net/about) - Gwern's own explanation of the site
- [gwern.net/design](https://gwern.net/design) - Design philosophy and decisions
- [gwern.net/changelog](https://gwern.net/changelog) - Site change history
