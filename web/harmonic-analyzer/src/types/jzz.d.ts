declare module 'jzz' {
  export interface MidiOut {
    noteOn(channel: number, note: number, velocity?: number): MidiOut;
    noteOff(channel: number, note: number, velocity?: number): MidiOut;
    close(): void;
  }

  export interface MidiOpenResult {
    and(callback: (this: MidiOut) => void): MidiOpenResult;
    or(callback: (error: unknown) => void): MidiOpenResult;
  }

  export interface JZZInstance {
    openMidiOut(name?: string): MidiOpenResult;
  }

  export interface JZZStatic {
    (): JZZInstance;
    synth: {
      Tiny: {
        register(name?: string): void;
      };
    };
  }

  const JZZ: JZZStatic;
  export default JZZ;
}



declare module 'jzz-synth-tiny' {
  import { JZZStatic } from 'jzz';
  export type TinyFn = (jzz: JZZStatic) => void;
  export const Tiny: TinyFn;
  const TinyDefault: TinyFn;
  export default TinyDefault;
}
