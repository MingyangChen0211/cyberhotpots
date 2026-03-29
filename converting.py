# created by: Chen, Mingyang
import os

# function convert all data into R utf-8 csv
def convert_to_utf8(folder, original_encoding='gbk', extensions=(".txt", ".csv", ".md")):
    extensions_lower = tuple(ext.lower() for ext in extensions)
    for root, _, files in os.walk(folder):
        for file in files:
            if file.lower().endswith(extensions_lower):
                path = os.path.join(root, file)
                try:
                    with open(path, 'r', encoding=original_encoding, errors='replace') as f:
                        content = f.read()
                    with open(path, 'w', encoding='utf-8-sig') as f:
                        f.write(content)
                    print(f"converting success: {path}")
                except Exception as e:
                    print(f"skip {path}，reason: {e}")


# converting
convert_to_utf8("your path", original_encoding='gb18030')
# you can change society into your own path.
