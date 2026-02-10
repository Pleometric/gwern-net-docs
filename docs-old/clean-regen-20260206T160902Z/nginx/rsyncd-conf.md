---
sidebar_position: 3
---

# rsyncd.conf

**Path:** `nginx/rsyncd.conf` | **Language:** Config

Rsync daemon configuration providing read-only public access to large dataset mirrors hosted by gwern.net.

---

## Overview

This configuration file defines four rsync modules that mirror large public datasets for research and preservation purposes. The rsync daemon provides an efficient, bandwidth-friendly way to distribute multi-gigabyte archives to researchers and mirrors worldwide.

All modules are configured as read-only with generous 6000-second (100-minute) timeouts to accommodate slow connections and large file transfers. The configuration supports large-scale data distribution including the Danbooru2021 anime image dataset (5.9TB), deprecated neural network resources, and the ThisWaifuDoesNotExist.net archive.

Two of the four modules (`dnmarchives` and `biggan`) are marked as deprecated with scheduled removal dates, reflecting evolving storage priorities and the migration of content to web-based access.

## Key Directives/Settings

### Global Configuration

- `log file = /var/log/rsyncd.log` - Centralized logging for all rsync daemon activity

### Modules

#### danbooru2021

**Large-scale anime image dataset mirror**

- `path = /home/danbooru/torrent/danbooru2021/`
- `comment = Danbooru2021 mirror (see <https://gwern.net/danbooru2021>)`
- `list = yes` - Module appears in module listings
- `read only = yes` - No uploads permitted
- `timeout = 6000` - 100-minute timeout for large transfers

The Danbooru2021 dataset is a comprehensive archive of ~5.9 million tagged anime/manga images from the Danbooru imageboard, totaling approximately 5.9TB. It's used extensively in machine learning research, particularly for training generative models like GANs and diffusion models. This rsync mirror provides an alternative to BitTorrent for researchers who need reliable, resumable access.

#### dnmarchives

**Darknet market research archive (DEPRECATED)**

- `path = /home/danbooru/torrent/dnmarchives/`
- `comment = DNM Archives (2011–2015) mirror ... !WARNING!: This rsync mirror is deprecated; use <https://gwern.net/doc/darknet-market/dnm-archive/file/index> instead. Do not use it; mirror any files you need. Files will be removed on 2026-01-01.`
- `read only = yes`
- `timeout = 6000`

Archive of darknet marketplace data (forum posts, vendor listings, etc.) from 2011-2015, used for research into cryptomarkets and online drug trafficking. The deprecation warning indicates migration to web-based access, with rsync scheduled for removal on January 1, 2026.

#### biggan

**Neural network model checkpoints and datasets (DEPRECATED)**

- `path = /home/danbooru/biggan/`
- `comment = Neural Net resources: model checkpoints & datasets (BigGAN, GPT-2, StyleGAN, etc). !WARNING!: this rsync mirror is deprecated! Do not use it; mirror any files you need. Files will start being removed on 2026-01-01.`
- `read only = yes`
- `timeout = 6000`

Collection of pre-trained model weights and datasets for various neural networks (BigGAN, GPT-2, StyleGAN, etc.). Also marked deprecated with removal starting January 1, 2026. Likely being phased out due to:
- Storage costs
- Availability of models elsewhere (Hugging Face, official repositories)
- Shift toward newer model architectures

#### twdne

**ThisWaifuDoesNotExist.net full site mirror**

- `path = /home/gwern/thiswaifudoesnotexist.net/`
- `comment = ThisWaifuDoesNotExist.net full site mirror (all JPGs and text files). See <https://gwern.net/twdne> for background.`
- `read only = yes`
- `timeout = 6000`

Complete archive of the ThisWaifuDoesNotExist.net generated anime portraits. Unlike the deprecated modules, this one has no removal warning, suggesting it's considered a permanent archive. Provides an alternative to HTTP access (see `twdne.conf`) for bulk downloading.

## Special Features

### Read-Only Public Access

All modules are configured as public, read-only mirrors with no authentication required. This supports the open science and digital preservation missions of gwern.net.

### Extended Timeouts

The 6000-second (100-minute) timeout is significantly longer than rsync's default 60 seconds, accommodating:
- Large file transfers (some files are multi-GB)
- Slow or unstable international connections
- Batch downloads of entire datasets
- Mirror operations that sync thousands of files

### Deprecation Notices in Comments

The configuration embeds user-facing warnings directly in the `comment` field, which appears when users list available modules. This is an elegant way to communicate deprecation without additional documentation, as the warnings appear inline during rsync operations:

```bash
$ rsync gwern.net::
dnmarchives     DNM Archives (2011–2015) mirror ... !WARNING!: This rsync mirror is deprecated...
```

### Cross-Service Integration

The modules complement HTTP access:
- `twdne` module pairs with `twdne.conf` nginx configuration
- `danbooru2021` supplements BitTorrent distribution
- Rsync provides resumable transfers and efficient differential updates

## Use Cases

### Research Dataset Distribution

The primary use case is providing large machine learning datasets to researchers:
- Danbooru2021 for training generative anime models
- BigGAN/GPT-2/StyleGAN weights for fine-tuning or analysis
- DNM archives for sociological/criminological research

### Mirroring and Preservation

Rsync's efficient delta-transfer algorithm makes it ideal for:
- Creating complete local mirrors of the datasets
- Periodically syncing updates without re-downloading unchanged files
- Distributed preservation (multiple institutions can easily maintain copies)

### Alternative to Web Downloads

Rsync offers advantages over HTTP:
- Built-in resume capability for interrupted transfers
- Efficient recursive directory syncing
- Bandwidth throttling and scheduling support
- Better handling of thousands of small files

## Example Usage

```bash
# List available modules
rsync gwern.net::

# Download Danbooru2021 dataset (5.9TB)
rsync -avz --progress gwern.net::danbooru2021/ /local/path/

# Sync only new files
rsync -avz --progress --update gwern.net::danbooru2021/ /local/path/

# Download TWDNE archive
rsync -avz gwern.net::twdne/ ./thiswaifudoesnotexist/
```

---

## See Also

- [gwern.net.conf](/nginx/gwern-net-conf) - Main nginx configuration for gwern.net
- [twdne.conf](/nginx/twdne-conf) - HTTP access to TWDNE archive (complements rsync)
- [memoriam.sh](/nginx/memoriam-sh) - Memorial header script (different server component)
- [redirect-nginx](/nginx/redirect-nginx) - URL redirect rules
- [sync.sh](/backend/sync-sh) - Build/deploy script that may update rsync modules
