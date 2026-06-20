---
title: "twdne.conf"
description: "This is a straightforward nginx server block configuration for hosting the ThisWaifuDoesNotExist.net static file archive."
sidebar_position: 2
---

# twdne.conf

This is a straightforward nginx server block configuration for hosting the ThisWaifuDoesNotExist.net static file archive.

<div className="doc-meta">
  <div><strong>Path</strong><code>nginx/twdne.conf</code></div>
  <div><strong>Language</strong>Nginx</div>
  <div><strong>Lines</strong>19</div>
  <div><strong>Source</strong><a href="https://github.com/gwern/gwern.net/blob/406d3e423c5dd42f3d431d6fedd203de5d277a2f/nginx/twdne.conf">nginx/twdne.conf</a><br /><span className="source-link">at 406d3e423</span></div>
</div>

<div className="read-when">
<strong>Read this when</strong>
Use this page when tracing server routing, redirects, deployment configuration, or Nginx behavior around twdne.
</div>

## Overview

This is a straightforward nginx server block configuration for hosting the ThisWaifuDoesNotExist.net static file archive. TWDNE is a mirror of an AI-generated anime character portrait site, preserved as part of gwern.net's broader digital preservation efforts.

The configuration is deliberately simple compared to the main gwern.net config, serving only static files with directory browsing enabled. It handles both the primary domain and the `www` subdomain variant, and relies on nginx's default file serving behavior with minimal customization. The site consists entirely of pre-generated JPG images and descriptive text files.

## Key Directives/Settings

### Server Configuration

- `listen 80` - HTTP only (no HTTPS configured)
- `server_name thiswaifudoesnotexist.net www.thiswaifudoesnotexist.net` - Handles both variants
- `root /home/gwern/thiswaifudoesnotexist.net` - Document root containing static files

### Location Block

**Basic file serving:**
```nginx
location / {
    try_files $uri $uri/ =404;
    autoindex on;
}
```

- `try_files $uri $uri/ =404` - Attempt to serve as file, then directory, then 404
- `autoindex on` - Enable directory listings for browsing the image archive

## Special Features

### Directory Browsing

Unlike the main gwern.net site which generates custom HTML indexes for most directories, TWDNE relies on nginx's built-in `autoindex` module to provide simple, functional directory listings. This is appropriate for the archive's purpose: allowing users and automated tools to browse and download the preserved images.

### No SSL/TLS

The configuration operates on port 80 only without HTTPS. This is likely acceptable for a static archive mirror that doesn't handle sensitive data or user input. The files are also available via rsync (see `rsyncd.conf`), providing an alternative access method.

### Minimal Complexity

The deliberately minimal configuration stands in stark contrast to the main gwern.net config's 1,100+ lines. This reflects the different use case: TWDNE is a frozen archive with no dynamic content, annotations, or complex URL routing requirements.

## Background: ThisWaifuDoesNotExist.net

ThisWaifuDoesNotExist.net was an AI-generated anime character portrait site that used StyleGAN or similar generative adversarial network (GAN) models to create infinite variations of anime-style character art. The site is part of the broader "ThisXDoesNotExist" family of GAN demonstration sites.

Gwern Branwen archived the site as part of his research into and documentation of AI-generated art, particularly in the anime and manga domain. The preserved version serves as:
- A research resource for studying early GAN-based artwork
- Historical documentation of AI art capabilities at a specific point in time
- A public archive accessible via both HTTP and rsync

For more background, see the main gwern.net article at `https://gwern.net/twdne`.

---

<details className="generated-section">
<summary>See Also</summary>

- [gwern.net.conf](/nginx/gwern-net-conf) - Main site nginx configuration (much more complex)
- [rsyncd.conf](/nginx/rsyncd-conf) - Rsync daemon providing TWDNE mirror access
- [memoriam.sh](/nginx/memoriam-sh) - Memorial header (not used in TWDNE)
- [redirect-nginx](/nginx/redirect-nginx) - URL redirects (not used in TWDNE)
- [sync.sh](/backend/sync-sh) - Build orchestrator that may handle TWDNE deployment
</details>
