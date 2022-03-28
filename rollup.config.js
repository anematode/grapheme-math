import typescript from '@rollup/plugin-typescript';

export default {
  input: 'src/index.ts',
  output: {
    //sourcemap: true,
    format: 'umd',
    file: 'build/main.js',
    name: 'Grapheme'
  },
  plugins: [typescript()]
};
