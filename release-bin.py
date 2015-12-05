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
    subprocess.check_call(['stack', 'build'], env=new_env)
    p = subprocess.check_output(['stack', 'path', '--local-install-root'],
                                env=new_env)
    path = pathlib.Path(p.decode('utf8').strip())
    ghc_version = path.name
    dest = dist.joinpath('shc-linux-x64-{}'.format(ghc_version))
    subprocess.check_call(['tar', '-jcvf', str(dest) + '.tar.bz2', 'shc'],
                          cwd=str(path.joinpath('bin')))
