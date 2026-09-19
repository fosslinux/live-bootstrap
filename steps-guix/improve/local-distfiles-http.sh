#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later

distfiles="${DISTFILES:-/external/distfiles}"
distfiles_http_host="127.0.0.1"
distfiles_http_port="38445"
distfiles_http_base_url="http://${distfiles_http_host}:${distfiles_http_port}"
distfiles_http_pid=""
distfiles_http_log="/tmp/distfiles-httpd.log"

stop_distfiles_http_server() {
    if [ -n "${distfiles_http_pid}" ] && kill -0 "${distfiles_http_pid}" >/dev/null 2>&1; then
        kill "${distfiles_http_pid}" >/dev/null 2>&1 || true
        wait "${distfiles_http_pid}" >/dev/null 2>&1 || true
    fi
}

start_distfiles_http_server() {
    if [ ! -d "${distfiles}" ]; then
        echo "Distfiles directory is missing: ${distfiles}" >&2
        exit 1
    fi

    if ! command -v python3 >/dev/null 2>&1; then
        echo "python3 is required to serve local distfiles over HTTP." >&2
        exit 1
    fi

    rm -f "${distfiles_http_log}"

    echo "Starting local distfiles HTTP server at ${distfiles_http_base_url}"
    python3 -m http.server "${distfiles_http_port}" \
        --bind "${distfiles_http_host}" \
        --directory "${distfiles}" \
        >"${distfiles_http_log}" 2>&1 &
    distfiles_http_pid="$!"

    retry=0
    while [ "${retry}" -lt 30 ]; do
        if ! kill -0 "${distfiles_http_pid}" >/dev/null 2>&1; then
            echo "Local distfiles HTTP server exited unexpectedly." >&2
            cat "${distfiles_http_log}" >&2 || true
            exit 1
        fi
        if python3 -c "import urllib.request; urllib.request.urlopen('${distfiles_http_base_url}/', timeout=2).read(1)" \
            >/dev/null 2>&1; then
            return
        fi
        retry=$((retry + 1))
        sleep 1
    done

    echo "Timed out waiting for local distfiles HTTP server: ${distfiles_http_base_url}" >&2
    cat "${distfiles_http_log}" >&2 || true
    exit 1
}
