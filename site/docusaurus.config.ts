import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';
import type {PluginOptions as SearchPluginOptions} from '@easyops-cn/docusaurus-search-local';

const config: Config = {
  title: "Swimm Docs",
  tagline: "Get started in minutes",
  favicon: 'img/favicon.ico',

  // Set the production url of your site here
  url: "https://example.test",
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: "/",

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  // organizationName: 'facebook', // Usually your GitHub org/user name.
  // projectName: 'docusaurus', // Usually your repo name.

  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  markdown: {
    format: 'detect',
    mermaid: true,
  },

  presets: [
    [
      'classic',
      {
        docs: {
          routeBasePath: '/',
          sidebarPath: './sidebars.ts',
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          // editUrl:
          //  'https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/',
        },
        blog: false,
      } satisfies Preset.Options,
    ],
  ],

  themes: [
    '@swimm',
    '@docusaurus/theme-mermaid',
    [
      '@easyops-cn/docusaurus-search-local',
      {
        hashed: true,
        indexBlog: false,
        docsRouteBasePath: '/',
      } satisfies SearchPluginOptions,
    ],
  ],

  themeConfig: {
    // Replace with your project's social card
    image: 'img/swimm-social-card.png',
    navbar: {
      logo: {
        alt: 'Logo',
        src: 'img/logo.svg',
        href: 'https://app.swimm.io/',
      },
      items: [
        {
          href: '/',
          position: 'left',
          label: "Swimm Docs",
        },
        {
          type: 'docSidebar',
          sidebarId: 'docsSidebar',
          position: 'left',
          label: 'Docs',
        },
      ],
    },
    footer: {
      links: [],
      copyright: `Swimm Docs. Built with Docusaurus and powered by <a href="https://app.swimm.io" target="_blank">Swimm</a>✨.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
    },
    colorMode: {
      respectPrefersColorScheme: true,
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
