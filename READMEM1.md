Recent revisions of Nixpkgs provide good support for Apple silicon (aarch64-darwin), however not 100% of packages work natively as of writing.

These simple instructions will guide you through installing Rosetta 2 and configuring Nix to enable building and running of non-native x86_64-darwin packages.

1. Install Rosetta 2

COPY
$ sudo softwareupdate --install-rosetta --agree-to-license
With Rosetta 2 installed, your machine is now capable of running programs built for x86_64-darwin.

2. Configure Nix's extra-platforms option
Nix doesn't know about Rosetta 2, so we need to tell it we're capable of building for x86_64-darwin.


COPY
$ sudo --edit /etc/nix/nix.conf

COPY
# /etc/nix/nix.conf

build-users-group = nixbld
extra-platforms = x86_64-darwin aarch64-darwin
3. Override Nixpkgs' default system
By default, system defaults to builtins.currentSystem, which is aarch64-darwin on machines with Apple silicon. We can override this in order to get packages for x86_64-darwin.


COPY
# hello.nix

let
  pkgs = import <nixpkgs> { system = "x86_64-darwin"; };

in
pkgs.hello
4. Build and run x86_64-darwin packages

COPY
$ nix build --file hello.nix

$ file ./result/bin/hello
./result/bin/hello: Mach-O 64-bit executable x86_64

$ ./result/bin/hello
Hello, world!
