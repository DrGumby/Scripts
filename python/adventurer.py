from armour import Armour
from weapon import Weapon
from item import Item

class Adventurer (object):
    name = ""
    race = ""
    aclass = ""
    items = {}
    weapons = {}
    armour = {}
    cash = 0
    hp = 0

    def __init__(self, name, race, aclass, items, weapons, armour, cash, hp):
        self.name = name
        self.race = race
        self.aclass = aclass
        self.items = items
        self.weapons = weapons
        self.armour = armour
        self.cash = cash
        self.hp = hp

    def show_weapons(self):
        for i in self.weapons:
            i.show()

    def show_armour(self):
        for i in self.armour:
            i.show()

    def show_items(self):
        for i in self.items:
            i.show()

    def show(self):
        print(
            "Name: {}\nRace: {}\nClass: {}\n".format(
                self.name, self.race, self.aclass))
        self.show_weapons()
        self.show_armour()
        self.show_items()
        print(
            "Cash: {}\nHP: {}\n".format(
                self.cash, self.hp))
