import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const siteUrl = 'https://gwern.pleometric.net';
const siteTitle = 'gwern.net Codebase Documentation';
const siteDescription =
  'Unofficial technical documentation for the gwern.net Hakyll, Pandoc, Haskell, JavaScript, annotation, popup, and static-site architecture.';
const siteImage = 'img/social-card.png';

const config: Config = {
  title: siteTitle,
  tagline: siteDescription,
  favicon: 'favicon.ico',
  headTags: [
    {tagName: 'link', attributes: {rel: 'apple-touch-icon', sizes: '180x180', href: '/apple-touch-icon.png'}},
    {tagName: 'link', attributes: {rel: 'icon', type: 'image/png', sizes: '32x32', href: '/favicon-32x32.png'}},
    {tagName: 'link', attributes: {rel: 'icon', type: 'image/png', sizes: '16x16', href: '/favicon-16x16.png'}},
    {tagName: 'link', attributes: {rel: 'manifest', href: '/site.webmanifest'}},
  ],

  future: {
    v4: true,
  },

  url: siteUrl,
  baseUrl: '/',

  onBrokenLinks: 'warn',

  markdown: {
    mermaid: true,
    hooks: {
      onBrokenMarkdownLinks: 'warn',
    },
  },

  themes: ['@docusaurus/theme-mermaid'],

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
          routeBasePath: '/',
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    // Open Graph social card image (for link previews)
    image: siteImage,
    metadata: [
      {name: 'description', content: siteDescription},
      {property: 'og:site_name', content: siteTitle},
      {property: 'og:title', content: siteTitle},
      {property: 'og:description', content: siteDescription},
      {property: 'og:type', content: 'website'},
      {property: 'og:image', content: `${siteUrl}/${siteImage}`},
      {name: 'twitter:card', content: 'summary_large_image'},
      {name: 'twitter:image', content: `${siteUrl}/${siteImage}`},
    ],
    tableOfContents: {
      minHeadingLevel: 2,
      maxHeadingLevel: 4,
    },
    colorMode: {
      respectPrefersColorScheme: true,
    },
    navbar: {
      title: 'gwern.net docs',
      items: [
        {
          href: 'https://gwern.net',
          label: 'gwern.net',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'light',
      links: [
        {
          title: 'Docs',
          items: [
            { label: 'Overview', to: '/' },
            { label: 'Frontend', to: '/frontend/initial-js' },
            { label: 'Backend', to: '/backend/link-metadata-hs' },
          ],
        },
        {
          title: 'External',
          items: [
            { label: 'gwern.net', href: 'https://gwern.net' },
            { label: 'gwern.net source code', href: 'https://github.com/gwern/gwern.net' },
            { label: 'Documentation repository', href: 'https://github.com/Pleometric/gwern-net-docs' },
          ],
        },
        {
          title: 'Credits',
          items: [
            { label: 'Gwern Branwen', href: 'https://github.com/gwern' },
            { label: 'Said Achmiz', href: 'https://github.com/achmizs' },
          ],
        },
      ],
      copyright: `Unofficial documentation of the gwern.net <a href="https://github.com/gwern/gwern.net">codebase</a> by <a href="https://pleometric.net">pleometric</a>.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['haskell', 'bash'],
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
