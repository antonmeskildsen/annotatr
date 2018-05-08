from flask import Flask
from flask import jsonify
from flask import request

import json

app = Flask(__name__)

@app.route('/track/<path:filename>')
def get_track(filename):
    response = app.response_class(
        response=open('trackfiles/' + filename).read(),
        status=200,
        mimetype='application/json'
    )
    return response

@app.route('/track/<path:filename>', methods=['POST'])
def update_track(filename):
    content = request.get_json()
    f = open('trackfiles/' + filename, "w")
    f.write(json.dumps(content))
    return jsonify({ })