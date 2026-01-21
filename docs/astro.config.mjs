// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';

// Load Quant grammar from the extension directory
const quantGrammar = JSON.parse(
	readFileSync(fileURLToPath(new URL('../extension/vscode/quant-highlighter/syntaxes/quant.tmLanguage.json', import.meta.url)), 'utf-8')
);

// Add required properties for Shiki
quantGrammar.name = 'quant';
quantGrammar.aliases = ['qa'];

// https://astro.build/config
export default defineConfig({
	integrations: [
		starlight({
			title: 'Quant Language',
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/Sigmapitech/glados' }],
			expressiveCode: {
				themes: ['github-dark', 'github-light'],
				shiki: {
					langs: [quantGrammar],
				},
			},
			sidebar: [
				{
					label: 'Guides',
					items: [
						{ label: 'Getting Started', slug: 'guides/getting-started' },
						{ label: 'Language Basics', slug: 'guides/language-basics' },
						{ label: 'Control Flow', slug: 'guides/control-flow' },
						{ label: 'Arrays', slug: 'guides/arrays' },
					],
				},
				{
					label: 'Reference',
					items: [
						{ label: 'Types', slug: 'reference/types' },
						{ label: 'Operators', slug: 'reference/operators' },
					],
				},
				{
					label: 'POC',
					autogenerate: { directory: 'poc' },
				},
				{
					label: 'Studies',
					items: [
						{ label: 'Documentation System Comparison', slug: 'studies/doc' },
						{ label: 'Parsing libraries', slug: 'studies/parser' },
						{ label: 'Data Persistence', slug: 'studies/persistence' }
					]
				}
			],
		}),
	],
});
