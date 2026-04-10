#!/usr/bin/env python3
from __future__ import annotations

import argparse
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path


WEBSITE_DIR = Path(__file__).resolve().parent


class WebsiteHandler(SimpleHTTPRequestHandler):
    def end_headers(self) -> None:
        # COOP/COEP only needed for playground (SharedArrayBuffer)
        if self.path.startswith("/playground"):
            self.send_header("Cross-Origin-Opener-Policy", "same-origin")
            self.send_header("Cross-Origin-Embedder-Policy", "require-corp")
            self.send_header("Cross-Origin-Resource-Policy", "same-origin")
        self.send_header("Cache-Control", "no-store")
        super().end_headers()


def main() -> None:
    parser = argparse.ArgumentParser(description="Serve the Aver website with isolation headers for playground.")
    parser.add_argument("port", nargs="?", type=int, default=4173)
    args = parser.parse_args()

    handler = partial(WebsiteHandler, directory=str(WEBSITE_DIR))
    server = ThreadingHTTPServer(("127.0.0.1", args.port), handler)

    print(f"Aver website: http://127.0.0.1:{args.port}")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()


if __name__ == "__main__":
    main()
