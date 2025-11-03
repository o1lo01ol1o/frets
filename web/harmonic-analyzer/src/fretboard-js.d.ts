declare module "@moonwave99/fretboard.js" {
  export interface FretboardOptions {
    el: HTMLElement;
    strings?: number;
    frets?: number;
    tuning?: string[];
    width?: number;
    height?: number;
    dotSize?: number;
  }

  export interface FretboardMarker {
    string: number;
    fret: number;
    label?: string;
    color?: string;
  }

  export default class Fretboard {
    constructor(options: FretboardOptions);
    render(markers: FretboardMarker[]): void;
  }
}
