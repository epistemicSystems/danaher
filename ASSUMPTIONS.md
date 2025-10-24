# ASSUMPTIONS

- Live breath proxy is derived from the average luminance of a manually selected region of interest (defaulting to the central 50%) until we have a trained chest segmentation model.
- WebGPU is available on the target desktop environment; we surface an error state if it is absent.
- Event persistence relies on the renderer's `localStorage` until a dedicated file-backed log is implemented; desktop environments without persistent web storage will lose session history between runs.
- Clipboard export of events assumes the host environment exposes `navigator.clipboard.writeText`; when unavailable the UI surfaces an error toast instead of attempting a fallback.
