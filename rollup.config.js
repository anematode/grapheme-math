import typescript from '@rollup/plugin-typescript';

export default {
  input: 'src/index.ts',
  output: {
    sourcemap: true,
    format: 'esm',
    file: 'build/main.js'
  },
  plugins: [typescript()]
};
