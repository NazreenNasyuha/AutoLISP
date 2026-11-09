/* ============================================================================
 * JRK AutoCAD Smart XLS-to-CSV Bridge — Tailwind CSS Configuration
 * ============================================================================ */

tailwind.config = {
  darkMode: 'class',
  theme: {
    extend: {
      colors: {
        /* Theme-aware surface palette (switches automatically between Dark & Light mode) */
        cadslate: {
          950: 'rgb(var(--s-950) / <alpha-value>)',
          900: 'rgb(var(--s-900) / <alpha-value>)',
          850: 'rgb(var(--s-850) / <alpha-value>)',
          800: 'rgb(var(--s-800) / <alpha-value>)',
          750: 'rgb(var(--s-750) / <alpha-value>)',
          700: 'rgb(var(--s-700) / <alpha-value>)',
          600: '#3d5a4b',
          500: '#527a66',
          400: '#6b9680',
        },
        /* Official Jurutera JRK Sdn. Bhd. Corporate Green (#00B050) */
        cadcyan: {
          300: '#4ade80',
          400: '#00c85b',
          500: '#00b050',
          600: '#009142',
          700: '#007535',
          900: '#064e28',
          950: '#032b16',
        },
        cademerald: {
          400: '#34d399',
          500: '#00b050',
          600: '#059669',
        },
      },
      fontFamily: {
        sans: ['Inter', 'system-ui', 'sans-serif'],
        mono: ['"JetBrains Mono"', 'Consolas', 'monospace'],
        serif: ['"Libre Baskerville"', 'Georgia', 'serif'],
      },
    },
  },
};
