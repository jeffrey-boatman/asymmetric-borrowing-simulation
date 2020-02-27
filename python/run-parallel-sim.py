import paramiko
import getpass
import sys

un = input('username: ')
pw = getpass.getpass()
nice = input('nice: ')

nice_options = list(range(1, 20))
nice_options = [str(num) for num in nice_options]
if str(nice) not in nice_options:
  nice = '19'

servers = ['carbon', 'cesium', 'chromium' , 'potassium', 'silicon']
# servers = ['carbon', 'chromium' , 'potassium', 'silicon']
server_nums = [str(i + 1) for i in range(len(servers))]

#for server, server_num in zip(servers, server_nums):
#  print(server, server_num)

# servers = ['carbon', 'cesium', 'potassium' , 'silicon']

command = "nohup nice +" + nice + " R CMD BATCH --vanilla sim.R &"

def runsim(server, command):
  ssh = paramiko.SSHClient()
  ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
  ssh.connect(server + '.biostat.umn.edu', username = un, password = pw)
  ssh.exec_command("cd sim/asymmetric-borrowing/R; " + command)
  ssh.close()

try:
  for server, server_num in zip(servers, server_nums):
    command = "nohup nice +" + nice + " R CMD BATCH --vanilla sim.R Rout/sim_" + server_num + ".Rout &"
    runsim(server, command)
    #print(command)
    print("jobs submitted to %s." %server)
except:
  print('ssh failed. Check username/password, code directory and/or internet connection.')
