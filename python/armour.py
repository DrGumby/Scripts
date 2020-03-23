class Armour (object):
    name = ""
    ac = 0
    check = 0

    def __init__(self, name, ac, check):
        self.name = name
        self.ac = ac
        self.check = check

    def show(self):
        print(
            "Name: {}\nAC: {}\nCheck: {}\n".format(
                self.name, self.ac, self.check))
