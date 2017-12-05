from flask import render_template, request, g, send_from_directory, Flask, send_file
from werkzeug import secure_filename
#from .forms import LoginForm
import os,sqlite3,string,random,csv,requests,glob,smtplib,re
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText



app = Flask(__name__)

DATABASE = "/home/ratanond/Desktop/Masters_Project/sCCA_Desktop/tool.db" 

def connect_to_database():
    return sqlite3.connect(DATABASE)

def get_db():
    db = getattr(g, '_database', None)
    if db is None:
        db = g._database = connect_to_database()
        db.text_factory = str
    return db

@app.route('/')
def uploadcca():
    return render_template("upload_demonstr.html")


@app.teardown_appcontext
def close_connection(exception):
    db = getattr(g, '_database', None)
    if db is not None:
        db.close()

def save_submission(query, args):
	c = get_db().cursor();
	c.execute(query, args)
	get_db().commit()
	id = c.lastrowid
	c.close()
	return id

def notify(name, email, project, description, id):
	msg = MIMEMultipart('alternative')
	msg['Subject'] = "CTEHR: Thank you for your submission!"
	msg['From']    = "CTEHR <CTEHR@cvm.tamu.edu>"
	msg['To']      = email

	
	jurl = "ccajobs"
	
	#html = "<p><em>Your submission ID is: <strong>"+id+"</strong>. Please use the following address to monitor the status of the job.</p><p></em><a href='http://sequencer.tamu.edu/compute/"+jurl+"?id="+id+"'>http://sequencer.tamu.edu/compute/"+jurl+"?id="+id+"</a></p><p>Submission details: </p><p><ul><li>Job Type: "+type+"</li><li>Name: "+name+"</li><li>Project: "+project+"</li><li>Description: "+description+"</li><ul></p>"
	#con = MIMEText(html, 'html')

	#username = 'sagarmehta89yhoo.com'
	#password = 'yZslfp-MJHiKlTfZE_0JpA'

	#msg.attach(con)
	#s = smtplib.SMTP('smtp.mandrillapp.com', 587)
	#s.login(username, password)
	#s.sendmail(msg['From'], msg['To'], msg.as_string())
	#s.quit()

@app.route('/return-file/')
def return_file():
    return send_file('/home/ratanond/Desktop/Masters_Project/CCA/Tool/ccaresults/azxScores1.pdf')

@app.route('/return-file-2/')
def return_file_2():
    return send_file('/home/ratanond/Desktop/Masters_Project/CCA/Tool/ccaresults/azxScores2.pdf')

@app.route('/return-file-3/')
def return_file_3():
    return send_file('/home/ratanond/Desktop/Masters_Project/CCA/Tool/ccaresults/Scores.csv')

@app.route('/return-file-4/')
def return_file_4():
    return send_file('/home/ratanond/Desktop/Masters_Project/CCA/Tool/ccaresults/TopList.csv')


@app.route('/ccajobs')
def ccajobs():
    ccajobs = get_db().execute("SELECT status,name,email,project FROM ccajobs WHERE id = ?", [request.args.get('id')])
    return render_template('ccajobs.html', rows=ccajobs.fetchall(), id=request.args.get('id'))

@app.route('/importcca', methods= ['POST'])
def import_cca():
	get_db().execute("CREATE TABLE IF NOT EXISTS ccajobs(status TEXT, name TEXT, email TEXT, project TEXT, description TEXT, file1 TEXT DEFAULT 'none', file2 TEXT DEFAULT 'none',rand TEXT, id integer primary key autoincrement)")
	input1 = request.files['file1']	    
	input2 = request.files['file2']
	
	name = request.form.get('name')
	email = request.form.get('email')
	project = request.form.get('project')
	description = request.form.get('description')

	if input1 and allowed_file(input1.filename):
	   dir1 = "/home/ratanond/Desktop/Masters_Project/sCCA_Desktop/input_store/"
	   inputname1 = secure_filename(input1.filename)
	   input1.save(os.path.join(dir1, inputname1))
	    

	if file and allowed_file(input2.filename):
	   dir2 = "/home/ratanond/Desktop/Masters_Project/sCCA_Desktop/input_store/"
	   inputname2 = secure_filename(input2.filename)
	   input2.save(os.path.join(dir2, inputname2))

	    

	
	lid = save_submission("INSERT INTO ccajobs (status, name, email, project, description,file1,file2, rand) VALUES (?,?,?,?,?,?,?,?)", ["Incomplete", name, email, project, description, inputname1,inputname2,"abc"]);
	
	notify(str(name), email, str(project), str(description), id)

	return render_template('job_success.html')

app.config['ALLOWED_EXTENSIONS'] = set(['txt', 'pdf', 'png', 'jpg', 'jpeg', 'gif','csv','TXT'])

def allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1] in app.config['ALLOWED_EXTENSIONS']



if __name__ == "__main__":
    app.run(debug=True)



