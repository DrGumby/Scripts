import sqlalchemy as db

class Database(object):
    conn = None
    meta = None
    engine = None

    class Tables(object):
        items = None
        players = None
        notes = None

    def __init__(self, db_file):
        """ Create a database connection
        :param db_file: Database file
        :return: Connection object or None
        """
        self.engine = db.create_engine('sqlite+pysqlite:///{}'.format(db_file))
        self.conn = self.engine.connect()
        self.meta = db.MetaData()
        self.Tables.players = db.Table(
            'Players', self.meta,
            db.Column('id', db.Integer, primary_key = True),
            db.Column('name', db.String),
            db.Column('hp', db.Integer),
            db.Column('money', db.Integer),
            db.Column('ammo', db.Integer)
        )

        self.Tables.items = db.Table(
            'Items', self.meta,
            db.Column('id', db.Integer, primary_key = True),
            db.Column('name', db.String),
            db.Column('count', db.Integer),
            db.Column('player', db.Integer, db.ForeignKey('Players.id'))
        )

        self.Tables.notes = db.Table(
            'Notes', self.meta,
            db.Column('id', db.Integer, primary_key = True),
            db.Column('name', db.String),
            db.Column('content', db.String),
            db.Column('player', db.Integer, db.ForeignKey('Players.id'))
        )

    def __del__(self):
        """
        Safely close db connection
        """
        self.conn.close()

    def create_db(self):
        """
        Create a database 
        """
        self.meta.create_all(self.engine)


