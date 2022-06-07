#!/usr/bin/env python3
import sys
from DictionaryServices import DCSCopyTextDefinition

def main():
    if len(sys.argv) == 1:
        exit(1)
    word = sys.argv[1].decode('utf-8')
    result = DCSCopyTextDefinition(None, word, (0, len(word)))
    print(result.encode('utf-8'))

if __name__ == '__main__':
    main()
