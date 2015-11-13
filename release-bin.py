#!/bin/python3

import os
import shutil
import pathlib
import subprocess


cwd = pathlib.Path.cwd()
dist = cwd.joinpath('dist')
dist.mkdir(exist_ok=True)

for config in cwd.glob('stack*.yaml'):
    new_env = os.environ.copy()
    new_env['STACK_YAML'] = str(config)
    p = subprocess.run(['stack', 'path', '--local-install-root'], check=True,
                       env=new_env, stdout=subprocess.PIPE).stdout
    path = pathlib.Path(p.decode('utf8').strip())
    ghc_version = path.name
    dest = dist.joinpath('shc-linux-x64-{}'.format(ghc_version))
    shutil.copy2(str(path.joinpath('bin', 'shc')), str(dest))
    subprocess.check_call(['tar', '-jcvf', str(dest) + '.tar.bz2', str(dest)])
