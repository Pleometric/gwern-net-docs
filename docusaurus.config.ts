import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const config: Config = {
  title: 'gwern.net unofficial docs',
  tagline: 'Technical documentation for the gwern.net codebase',
  favicon: 'img/favicon.ico',

  future: {
    v4: true,
  },

  url: 'https://gwern-analysis.example.com',
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
    tableOfContents: {
      minHeadingLevel: 2,
      maxHeadingLevel: 4,
    },
    colorMode: {
      respectPrefersColorScheme: true,
    },
    navbar: {
      title: 'gwern.net unofficial docs',
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
            { label: 'Source Code', href: 'https://github.com/gwern/gwern.net' },
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
