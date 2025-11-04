# /home/stalk/dotfiles

这个储存库是我自定义Guix系统配置文件的主页，它支持跨多台计算机提供一致的体验。

- Guix 官网：<https://guix.gnu.org>

我使用的是 GNU Guix 发行版，内核为 GNU/Linux。

# Installation

1. clone储存库

```shell
git clone https://github.com/Stalk-evolto/dotfiles.git $HOME/dotfiles
```

2. clone子仓库

```shell
git submodule init
git submodule update
```

3. 检查clone的仓库是否为正版

```shell
git fetch origin keyring:keyring
guix git authenticate cbc970677a439191b59d4787823ff447a154c45d \
  "1903 4003 F817 AABF AA97  0070 7303 8079 9D2B CDD0"
```

如果成功，终端会有如下显示

```shell
guix git: successfully authenticated commit cbc970677a439191b59d4787823ff447a154c45d
```

4. 使用`guix system`重新配置系统

```shell
guix system reconfigure -L $HOME/dotfiles $HOME/dotfiles/config/systems/system.scm
```

5. 使用`guix home`重新配置用户home

```shell
guix home reconfigure -L $HOME/dotfiles $HOME/dotfiles/config/home/home-config.scm
```

# License

Copyright © 2025 Stalk Evolto
Released under the [GPLv3 License](https://www.gnu.org/licenses/quick-guide-gplv3.html) unless otherwise specified by license files in subfolders.

# Mailing List

- Stalk Evolto : <stalk-evolto@outlook.com>
