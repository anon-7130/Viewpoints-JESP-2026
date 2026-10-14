from flask import Flask, render_template, request
import pandas as pd
from pathlib import Path
import uuid
import hashlib
import os
import random

app = Flask(__name__)


def data_submission(row, variable):
    if variable == "Agreement":
        return round(float(request.form.get(hashlib.sha1(row["Argument"].encode("UTF-8")).hexdigest() + "-agreement")), 5)
    elif variable == "Agreement.Contacts":
        return round(float(request.form.get(hashlib.sha1(row["Argument"].encode("UTF-8")).hexdigest() + "-agreement-contacts")), 5)


@app.route('/', methods=['GET'])
def index():
    user = request.args.get("user")
    return render_template("index.html", user=user)


@app.route('/XP/', methods=['GET'])
def xp():
    user = request.args.get("user")
    condition = int(request.args.get("condition"))
    this_folder = Path(__file__).parent.resolve()
    data_path = this_folder / "data.csv"
    rendered_path = this_folder / (
                "results/" + user + ".csv")
    data = pd.read_csv(data_path, encoding="utf-8")
    data = data.sample(frac=1).reset_index(drop=True)
    recorded_data = data[["Index", "Topic", "Argument", "Words"]]
    recorded_data["Condition_Share"] = int(request.args.get("condition"))
    recorded_data.index += 1
    recorded_data.to_csv(rendered_path, index_label="Order")
    os.utime(rendered_path, ns=(1, 1))
    return render_template("XP.html", arguments=data[["Topic", "Argument"]].values.tolist(), user=user, condition=condition)


@app.route('/questionnaire/', methods=['POST'])
def questionnaire():
    user = request.form.get("user")
    condition = int(request.form.get("condition"))
    dict_args = dict()
    this_folder = Path(__file__).parent.resolve()
    rendered_path = this_folder / (
            "results/" + user + ".csv")
    data = pd.read_csv(rendered_path, encoding="utf-8")
    list_share = [0]*len(data.index)
    if condition == 1:
        for key in request.form.getlist("argument-checkbox"):
            list_share[int(key)] = 1
    data["Share"] = list_share
    data.to_csv(rendered_path, index=False)
    os.utime(rendered_path, ns=(1, 1))
    for idx, row in data.iterrows():
        if row["Topic"] in dict_args.keys():
            dict_args[row["Topic"]].append((row["Argument"], hashlib.sha1(row["Argument"].encode("UTF-8")).hexdigest()))
        else:
            dict_args[row["Topic"]] = [(row["Argument"], hashlib.sha1(row["Argument"].encode("UTF-8")).hexdigest())]
    return render_template("questionnaire.html", user=user, arguments=dict_args, condition=condition)


@app.route('/ending/', methods=['POST'])
def ending():
    user = request.form.get("user")
    condition = int(request.form.get("condition"))
    this_folder = Path(__file__).parent.resolve()
    random_list_path = this_folder / ("random_list.csv")
    rendered_path = this_folder / (
            "results/" + user + ".csv")
    data = pd.read_csv(rendered_path, encoding="utf-8")
    data_random_list = pd.read_csv(random_list_path, encoding="utf-8")
    next_idx = data_random_list[pd.isnull(data_random_list.user)].first_valid_index()
    data_random_list.loc[next_idx, "user"] = user
    data_random_list.to_csv(random_list_path, index=False)
    data["Agreement"] = data.apply(data_submission, variable="Agreement", axis=1)
    data["Agreement.Contacts"] = data.apply(data_submission, variable="Agreement.Contacts", axis=1)
    if condition==1:
        data_random = data[data["Share"] == 1]
        data_random = data_random.sample(n = 1)

    data.to_csv(rendered_path, index=False)
    os.utime(rendered_path, ns=(1, 1))

    if condition==1:
        data_random = data[data["Share"] == 1]
        data_random = data_random.sample(n = 1)
        return render_template("ending.html", argument=data_random[["Topic", "Argument"]].values.tolist(), condition=condition)
    else:
        return render_template("ending.html", condition=condition)


@app.route('/instructions/', methods=['GET'])
def instructions():
    user = request.args.get("user")
    this_folder = Path(__file__).parent.resolve()
    random_list_path = this_folder / ("random_list.csv")
    data_random_list = pd.read_csv(random_list_path, encoding="utf-8")
    next_idx = data_random_list[pd.isnull(data_random_list.user)].first_valid_index()
    if(next_idx!=None):
        condition = data_random_list.loc[next_idx, "condition"]
    else:
        condition = 2
    return render_template("instructions.html", user=user, condition=condition)
