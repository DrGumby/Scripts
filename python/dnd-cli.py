import cmd
import colorama
import pickle
from adventurer import Adventurer
from armour import Armour
from weapon import Weapon
from item import Item
from item import Ammo

class DShell(cmd.Cmd):
    intro = "Welcome to D&D shell. Type help or ? to list commands."
    prompt = colorama.Style.BRIGHT + colorama.Fore.RED + '(D&D)>' + colorama.Style.RESET_ALL
    all_players = {}
    selected_player = None
    filename = ""

    def do_load(self, arg):
        self.filename = arg
        with open(arg, 'rb') as input:
            self.all_players = pickle.load(input)

    def do_save(self, arg=filename):
        with open(arg, 'wb') as output:
            pickle.dump(self.all_players, output, pickle.HIGHEST_PROTOCOL)

    def do_new(self, arg):
        args = arg.split()
        if args[0] == "player":
            player = create_player()
            self.all_players[player.name] = player

    def do_player(self, arg):
        self.selected_player = self.all_players[arg]
        self.prompt = colorama.Style.BRIGHT + colorama.Fore.GREEN + self.selected_player.name + " (D&D)>" + colorama.Style.RESET_ALL

    def do_show(self, arg):
        if self.selected_player:
            if arg == "name":
                print(self.selected_player.name)
            elif arg == "race":
                print(self.selected_player.race)
            elif arg == "class":
                print(self.selected_player.aclass)
            elif arg == "items":
                self.selected_player.show_items()
            elif arg == "weapons":
                self.selected_player.show_weapons()
            elif arg == "armour":
                self.selected_player.show_armour()
            elif arg == "cash":
                print(self.selected_player.cash)
            elif arg == "hp":
                print(self.selected_player.cash)
            elif arg == "all":
                self.selected_player.show()

    def do_echo(self, arg):
        print(arg)

    def do_EOF(self, arg):
        return True

def create_player():
    name = input("Enter name: ")
    race = input("Enter race: ")
    aclass = input("Enter class: ")
    items = create_items_multiple()
    weapons = create_weapons_multiple()
    armour = create_armours_multiple()
    cash = int(input("Enter cash amount: "))
    hp = int(input("Enter HP: "))
    return Adventurer(name, race, aclass, items, weapons, armour, cash, hp)


def create_item():
    name = input("Enter item name: ")
    desc = input("Enter item description: ")
    count = int(input("Enter item count: "))
    is_ammo = y_or_n("Should this item be considered as ammunition? [yes/no]")

    if is_ammo:
        return Ammo(name, desc, count)
    else:
        return Item(name, desc, count)

def create_items_multiple():
    items = {}
    while True:
        item = create_item()
        items[item.name] = item
        if not y_or_n("Enter another item? [yes/no]"):
            return items

def create_weapon():
    name = input("Enter weapon name: ")
    wtype = input("Enter weapon type: ")
    dice = int(input("Enter weapon dice: "))
    damage = int(input("Enter weapon damage: "))
    crit = input("Enter weapon crit: ")
    return Weapon(name, wtype, dice, damage, crit)

def create_weapons_multiple():
    weapons = {}
    while True:
        weapon = create_weapon()
        weapons[weapon.name] = weapon
        if not y_or_n("Enter another weapon? [y/n]"):
            return weapons

def create_armour():
    name = input("Enter armour name: ")
    ac = int(input("Enter armour AC: "))
    check = int(input("Enter armour check: "))
    return Armour(name, ac, check)

def create_armours_multiple():
    armours = {}
    while True:
        armour = create_armour()
        armours[armour.name] = armour
        if not y_or_n("Enter another armour? [y/n]"):
            return armours


def y_or_n(prompt):
    while True:
        query = input(prompt)
        answer = query[0].lower()
        if query == '' or not answer in ['y', 'n']:
            print("Please answer with yes or no!")
        else:
            if answer == 'y':
                return True
            else:
                return False


if __name__ == '__main__':
    colorama.init()
    DShell().cmdloop()
