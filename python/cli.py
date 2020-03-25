import cmd
import sqlalchemy as db
from colorama import Fore, Style
from pprint import pprint
from db import Database


def pretty_dict(item:dict):
    for i, j in item.items():
        print('{}:\t{}'.format(i.title(), j))

def new_player_interact():
    return {
        'name' : input("Enter name: "),
        'hp'   : input("Enter HP: "),
        'money': input("Enter money: "),
        'ammo' : input("Enter ammo: ") or None
    }

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

def new_item_interact():
    return {
        'name'   : input("Enter item name: "),
        'count'  : input("Enter item count: "),
        'player' : input("Who does this item belong to? ") or None
    }

def new_note_interact():
    return {
        'name'    : input("Enter note name: "),
        'content' : input("Enter note content: ")
    }

class GameShell(cmd.Cmd):
    intro = 'Welcome to the Dungeons and dragons shell. Type help or ? to list commands'
    prompt = Fore.RED + Style.BRIGHT + '(Game)> ' + Style.RESET_ALL
    database = None
    current_adv_id = None
    current_adv_name = None

    def do_connect(self, args):
        self.database = Database(args)

    def do_create(self, args):
        self.database.create_db()

    def do_EOF(self, args):
        return True
    
    def do_exit(self, args):
        return True

    def do_show(self, args):
        self.show(args)

    def do_add(self, args):
        self.add(args)

    def do_player(self, args):
        self.player(args)

    def do_heal(self, args):
        self.hp(int(args))

    def do_hit(self, args):
        self.hp(-int(args))

    def do_income(self, args):
        self.money(int(args))

    def do_spend(self, args):
        self.money(-int(args))

    def do_shoot(self, args):
        if not args:
            self.ammo(-1)
        else:
            self.ammo(-int(args))

    def do_craft(self, args):
        self.ammo(int(args))

    def show(self, args):
        args = args.split()
        arg = args[0]
        if arg == "tables":
            [*map(print, self.database.engine.table_names())]
        elif arg == "items":
            table = self.database.Tables.items
            if self.current_adv_id:
                self.show_query(table.select().where(table.c.player == self.current_adv_id or table.c.player == self.current_adv_name))
            else:
                self.show_query(table.select())
        elif arg == "players":
            self.show_query(self.database.Tables.players.select())
        elif arg == "self":
            table = self.database.Tables.players
            if not self.current_adv_id:
                print("No player selected!")
                return
            self.show_query(table.select().where(table.c.id == self.current_adv_id))
        elif arg == "notes":
            table = self.database.Tables.notes
            if not self.current_adv_id:
                print("No player selected!")
                return
            self.show_query(table.select().where(table.c.player == self.current_adv_id))
        
        
    def add(self, args):
        args = args.split()
        arg = args[0]
        if arg == "player":
            self.database.conn.execute(
                self.database.Tables.players.insert(None), [new_player_interact()]
            )
        elif arg == "item":
            self.database.conn.execute(
                self.database.Tables.items.insert(None), [new_item_interact()]
            )
        elif arg == "note":
            if not self.current_adv_id:
                print("No player selected!")
                return
            note = new_note_interact()
            note['player'] = self.current_adv_id
            self.database.conn.execute(
                self.database.Tables.notes.insert(None), [note]
            )
            
    def show_query(self, query):
        res = self.database.conn.execute(query)
        out = res.fetchall()
        for i in out:
            pretty_dict(dict(zip(i.keys(), i)))

    def hp(self, arg):
        if not self.current_adv_id:
            print("No player selected!")
            return

        table = self.database.Tables.players
        upd = table.update(None).where(table.c.id == self.current_adv_id).values(hp=table.c.hp+(int(arg)))
        self.database.conn.execute(upd)

    def money(self, arg):
        if not self.current_adv_id:
            print("No player selected!")
            return

        table = self.database.Tables.players
        upd = table.update(None).where(table.c.id == self.current_adv_id).values(money=table.c.money+(int(arg)))
        self.database.conn.execute(upd)

    def ammo(self, arg):
        if not self.current_adv_id:
            print("No player selected!")
            return

        table = self.database.Tables.players
        upd = table.update(None).where(table.c.id == self.current_adv_id).values(ammo=table.c.ammo+(int(arg)))
        self.database.conn.execute(upd)

    def player(self, args):
        table = self.database.Tables.players
        query = table.select().where(table.c.name == args)
        res = self.database.conn.execute(query)
        out = res.fetchone()
        if not out:
            print("No player found!")
            return
        
        out_d = dict(zip(out.keys(), out))
        self.current_adv_id = out_d['id']
        self.current_adv_name = out_d['name']
        self.prompt = Fore.GREEN + Style.BRIGHT + '({})> '.format(self.current_adv_name) + Style.RESET_ALL

if __name__ == '__main__':
    GameShell().cmdloop()