// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';

// Load Quant grammar from the extension directory
const quantGrammar = JSON.parse(
	readFileSync(fileURLToPath(new URL('../extension/vscode/quant-extension/syntaxes/quant.tmLanguage.json', import.meta.url)), 'utf-8')
);

// https://astro.build/config
export default defineConfig({
	integrations: [
		starlight({
			title: 'Quant Language',
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/Sigmapitech/glados' }],
			expressiveCode: {
				themes: ['github-dark', 'github-light'],
				customLanguages: [
					{
						...quantGrammar,
						id: 'quant',
						scopeName: 'source.quant',
						aliases: ['qa'],
					}
				],
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
			],
		}),
	],
});
