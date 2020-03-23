class Weapon (object):
    name = ""
    wtype = ""
    damage = 0
    dice = 0
    crit = ""

    def __init__(self, name, wtype, damage, dice, crit):
        self.name = name
        self.wtype = wtype
        self.damage = damage
        self.dice = dice
        self.crit = crit

    def show(self):
        print(
            "Name: {}\nType: {}\nDamage: {}\nDice: {}\nCrit: {}\n".format(
                self.name, self.wtype, self.damage, self.dice, self.crit))
