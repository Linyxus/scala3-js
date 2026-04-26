import hello from './hello.scala?raw';
import canvas from './canvas.scala?raw';
import spirograph from './spirograph.scala?raw';
import gameoflife from './gameoflife.scala?raw';

export interface Example {
  id: string;
  label: string;
  source: string;
}

export const examples: Example[] = [
  { id: 'hello', label: 'Hello World', source: hello },
  { id: 'canvas', label: 'Canvas Drawing', source: canvas },
  { id: 'spirograph', label: 'Spirograph', source: spirograph },
  { id: 'gameoflife', label: 'Game of Life', source: gameoflife },
];

export const defaultExample = 'canvas';
