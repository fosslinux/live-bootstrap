# SPDX-FileCopyrightText: 2025 reshi <reshil@reshlet.com>
# SPDX-License-Identifier: CC0-1.0

# We need a handwave to make ubuntu happy in order to run bwrap
# see https://etbe.coker.com.au/2024/04/24/ubuntu-24-04-bubblewrap/

# But, in some cases, it seems that 'ubuntu-latest' does not always 
#  'resolve' to 24.04, so we do some hand waves to check for
#  os version >= 24.04 (otherwise the build can die when
#  trying to apply bwrap.apparmor)

# Figure out current ubuntu version
# https://manpages.ubuntu.com/manpages/noble/man5/os-release.5.html
#
#  The /etc/os-release and /usr/lib/os-release files contain 
#  operating system identification data.
#
#  The format of os-release is a newline-separated list of 
#  environment-like shell-compatible variable assignments.
#
#  The file /etc/os-release takes precedence over /usr/lib/os-release. 
#  Applications should check for the former, 
#  and exclusively use its data if it exists, 
#  and only fall back to /usr/lib/os-release if it is missing.
#
#  VERSION_ID=
#    A lower-case string 
#    (mostly numeric, no spaces or other characters outside of 0-9,
#    a-z, ".", "_" and "-") 
#    identifying the operating system version,
#    excluding any OS name information or release code name, 
#    and suitable for processing by scripts
#    or usage in generated filenames.
#    This field is optional.
#
#    Examples: "VERSION_ID=17", "VERSION_ID=11.04".

# Check for /etc/os-release or fall back to /usr/lib/os-release
if [ -f /etc/os-release ]; then
  OS_RELEASE_FILE="/etc/os-release"
elif [ -f /usr/lib/os-release ]; then
  OS_RELEASE_FILE="/usr/lib/os-release"
else
  echo "Error: Neither /etc/os-release nor /usr/lib/os-release found."
  exit 1
fi

# Extract 'VERSION_ID=' line.
VERSION_ID_LINE=$(grep '^VERSION_ID=' "$OS_RELEASE_FILE")
if [ -z "$VERSION_ID_LINE" ]; then
  echo "Error: VERSION_ID not found in $OS_RELEASE_FILE."
  echo "Contents of $OS_RELEASE_FILE:"
  cat "$OS_RELEASE_FILE"
  exit 1
fi

# Extract major/minor version
if [[ "$VERSION_ID_LINE" =~ ^VERSION_ID=\"([0-9]+)\.([0-9]+)\"$ ]]; then
  # Matches 'VERSION_ID="major.minor"' (e.g., "24.04")
  MAJOR="${BASH_REMATCH[1]}"
  MINOR="${BASH_REMATCH[2]}"
  echo "Ubuntu version: $MAJOR.$MINOR"
elif [[ "$VERSION_ID_LINE" =~ ^VERSION_ID=\"([0-9]+)\"$ ]]; then
  # Matches 'VERSION_ID="major"' (e.g., "24")
  MAJOR="${BASH_REMATCH[1]}"
  MINOR="0"
  echo "Ubuntu version: $MAJOR.$MINOR (no minor version specified)"
else
  echo "Error: VERSION_ID is malformed in $OS_RELEASE_FILE."
  echo "VERSION_ID_LINE: \"$VERSION_ID_LINE\""
  exit 1
fi

# Check for version >= 24.04, do workaround if so
check_version_ge() {
  local major=$1
  local minor=$2
  (( MAJOR > major || (MAJOR == major && MINOR >= minor) ))
}
if check_version_ge 24 4; then
  echo "Ubuntu version is >= 24.04, deploying bwrap work-around..."
  sudo cp .github/workflows/bwrap.apparmor /etc/apparmor.d/bwrap || { 
    echo "Failed to copy AppArmor profile"; 
    exit 1; 
  }
  echo "Reloading AppArmor service..."
  sudo systemctl reload apparmor || {
    # error msg from 'systemctl reload apparmor' 
    #  suggests looking at the following...
    echo "Failed to reload AppArmor. Checking status..."; 
    systemctl status apparmor.service;
    echo "Checking logs...";
    journalctl -xeu apparmor.service;
    exit 1; 
  }
else
  echo "Ubuntu version is < 24.04, skipping bwrap work-around..."
fi
