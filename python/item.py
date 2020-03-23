from colorama import init, Fore, Style

class Item (object):
    name = ""
    desc = ""
    count = 0

    def __init__(self, name, description, count):
        self.name = name
        self.desc = description
        self.count = count

    def show(self):
        init()
        print(
            "{yellow}{bold}Item: {name}\n{reset}Description: {desc}\nCount: {count}".format(
                yellow=Fore.YELLOW,
                bold=Style.BRIGHT,
                name=self.name,
                reset=Style.RESET_ALL,
                desc=self.desc,
                count=self.count))

    def remove(self, count):
        self.count -= count

    def craft(self, count):
        self.count += count

class Ammo (Item):
    def __init__(self, name, description, count):
        super().__init__(name, description, count)

    def shoot(self, count=1):
        self.count -= count
