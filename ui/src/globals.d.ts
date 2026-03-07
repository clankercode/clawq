declare global {
  interface Window {
    DOMPurify: {
      sanitize(input: string): string;
    };
    hljs: {
      getLanguage(language: string): unknown;
      highlight(code: string, options: { language: string }): { value: string };
      highlightAuto(code: string): { value: string };
    };
    marked: {
      parse(markdown: string): string;
      setOptions(options: Record<string, unknown>): void;
    };
    mermaid?: {
      initialize(options: Record<string, unknown>): void;
      run(options?: Record<string, unknown>): Promise<void>;
    };
  }
}

export {};
