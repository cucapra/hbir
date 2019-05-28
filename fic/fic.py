import click
import tomlkit
import os
import shlex
import subprocess

CONFIG_FILENAME = 'fic.toml'


def load_config(path):
    """Load the configuration for a test at the given path.
    """
    parent = os.path.dirname(path)
    config_path = os.path.join(parent, CONFIG_FILENAME)
    if os.path.isfile(config_path):
        with open(config_path) as f:
            return tomlkit.loads(f.read())
    else:
        return None


def get_command(config, path):
    """Get the command to run for a given test.
    """
    parts = shlex.split(config['command'])
    return [
        p.format(filename=os.path.basename(path))
        for p in parts
    ]


def run_test(path):
    config = load_config(path)
    cmd = get_command(config, path)

    print(cmd)
    subprocess.call(cmd)


@click.command()
@click.argument('file', nargs=-1, type=click.Path(exists=True))
def fic(file):
    for path in file:
        run_test(path)


if __name__ == '__main__':
    fic()
