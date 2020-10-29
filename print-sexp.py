import sys
import re

indent_level = 0

while block := sys.stdin.read(128):
    while match := re.search(r'\(|\)+', block):
        text = block[:match.start()]
        print(text, end='')

        if match[0] == '(':
            print('\n' + ' '*indent_level + '(', end='')
            indent_level += 2
        else:
            print(match[0], end='')
            indent_level -= 2 * len(match[0])

        block = block[match.end():]

    print(block, end='')
print()