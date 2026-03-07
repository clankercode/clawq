import { defineConfig } from 'astro/config';
import mdx from '@astrojs/mdx';

export default defineConfig({
  site: 'https://clawq.org',
  server: { port: 32748 },
  integrations: [mdx()],
  markdown: {
    shikiConfig: {
      theme: 'css-variables',
    },
  },
  vite: {
    optimizeDeps: {
      include: ['mermaid'],
    },
  },
});
