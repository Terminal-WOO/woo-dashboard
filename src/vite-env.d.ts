/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_BACKEND_TYPE?: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}
