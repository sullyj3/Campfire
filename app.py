from flask import Flask, abort, request, g
from flask_cors import CORS
import json
import logging
import sys

import sqlite3

DB_PATH = 'my_db.db'

logging.getLogger('flask_cors').level = logging.DEBUG
logging.getLogger('flask_cors').level = logging.DEBUG

app = Flask(__name__)
CORS(app)



"""Note: sets row_factory to Row, allowing dict-like access to rows"""
def get_db():
    db = getattr(g, '_database', None)
    if db is None:
        db = g._database = sqlite3.connect(DB_PATH)
        db.execute("PRAGMA foreign_keys = ON")
        db.row_factory = sqlite3.Row
    return db

@app.teardown_appcontext
def teardown_db(exc):
    if exc is not None:
        print(exc)

    db = g.pop('db', None)

    if db is not None:
        db.close()

@app.route('/')
def hello_json():
    return json.dumps({"Hello,": "World!"})

@app.route('/stories')
def get_stories():
    db = get_db()
    cur = db.execute("SELECT storyid,title FROM Story")

    stories = cur.fetchall()
    # msg = [ {"storyid": s['storyid'], "title": s['title']} for s in stories]

    return serialize( [dict(s) for s in stories] )

@app.route('/story/<storyid>')
def get_story(storyid):
    db = get_db()
    cur = db.execute("SELECT storyid,title,content FROM Story WHERE storyid=?", (storyid, ))

    story = cur.fetchone()

    if story is None:
        msg = { "type": "could not find story",
                "id": storyid}
        return serialize(msg, "error")
    else:
        return serialize(dict(story))

def serialize(msg, response_type="data"):
    return json.dumps({response_type: msg})
