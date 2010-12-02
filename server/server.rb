#!/usr/bin/env ruby

require 'socket'
require 'sqlite3'
require 'digest/sha2'

$welcome="This is Scores Server v.0.7 (Running Ruby/#{RUBY_VERSION}@#{RUBY_PLATFORM})\n"

module DB
  include SQLite3
  def open(path)
    @db=Database.new(path.gsub(/([^\/]+)$/,'')+"ocamloid.db")
    if @db.execute("select name from sqlite_master where name='soloists'")==[]
      @db.execute("create table soloists(name text,score integer)")
    end
    if @db.execute("select name from sqlite_master where name='authentication'")==[]
      @db.execute("create table authentication(name text primary key,pass text)")
    end
  end
  def set(table,fields,values)
    q="insert into #{table} ("
    fields.each { |f| q+=f+',' }
    q.chop!
    q+=') values ('
    values.each { |v| q+="'"+v.to_s+"'," }
    q.chop!
    begin
      @db.execute(q+')')
      true
    rescue SQLException
      false
    end
  end
  def get(table,field,name)
    s=@db.execute("select #{field} from #{table} where name='#{name}'")
    if !s[0].nil?
      s[0][0]
    else
      -1
    end
  end
  def get_all(table)
    @db.execute("select * from #{table}")
  end
  def close
    @db.close
  end
end

module AUTH
  def generate(plain_pass)
    sugar=sweeten
    integrate(hash(plain_pass,sugar),sugar)
  end
  def authenticate?(plain_pass,stored_pass)
    pass_and_sugar=separate(stored_pass)
    pass_and_sugar[0]==hash(plain_pass.chomp,pass_and_sugar[1])
  end
  private
  def sweeten
    sugar=''
    64.times {sugar<<(i=rand(62);i+=((i<10)?48:((i<36)?55:61))).chr}
    sugar
  end
  def hash(pass,sugar)
    Digest::SHA512.hexdigest("#{pass}:#{sugar}")
  end
  def integrate(pass,sugar)
    pass+sugar
  end
  def separate(pass)
    [pass[0..127],pass[128..192]]
  end
end

module MSG
  def e1000
    "1000/I cannot resolve this request. Check your syntax.\n"
  end
  def e1001
    "1001/Not enough parameters, check your request syntax.\n"
  end
  def e1002
    "1002/Your name is not a proper string.\n"
  end
  def e1003
    "1003/Incorrect score format, are you trying to trick me? ;)\n"
  end
  def e1004
    "1004/No such record in database.\n"
  end
  def e1005
    "1005/You have to authenticate yourself first!\n"
  end
  def e1006
    "1006/Authentication failed, check your data.\n"
  end
  def e1007
    "1007/You are trying to modify someone other's score.\n"
  end
  def m1008
    "1008/Authentication complete. Welcome to Scores Server.\n"
  end
  def m1009
    "1009/User creation complete. We've automatically logged You in.\n"
  end
  def m1010
    "1010/Closing connection.\n"
  end
  def m1011
    "1011/Attempting to close connection.\n"
  end
  def m1100
    "1100/Stored.\n"
  end
  def m1110
    "1110/Retrieved.\n"
  end
  def e1020
    "1020/Account with that name already exists.\n"
  end
end

class Server
  include DB
  include MSG
  include AUTH
  def initialize(port=6666,path=File.expand_path($0))
    @webserver=TCPServer.new('127.0.0.1',port)
    open(path)
  end
  def destroy
    @webserver.close
    close
  end
  def accept
    begin
      @session=@webserver.accept_nonblock
      1
    rescue Errno::EAGAIN
      0
    rescue Exception
      -1
    end
  end
  def finish
    @session.close
  end
  def serve
    @session.print $welcome
    request=@session.gets
    if !request.nil?
      if request.include? "AUTH"
        r=request.gsub(/AUTH /,'').split(',')
        if r.length!=2
          @session.print e1001
        elsif !r[0].is_a?(String) or !r[1].is_a?(String)
          @session.print e1002
        else
          if (rr=get("authentication","pass",r[0]))!=-1
            if authenticate?(r[1],rr)
              serve_loop r[0]
            else
              @session.print e1006
            end
          else
            @session.print e1004
          end
        end
      elsif request.include? "CREATE"
        r=request.gsub(/CREATE /,'').split(',')
        if set("authentication",["name","pass"],[r[0],generate(r[1])])
          @session.print m1009
          serve_loop r[0]
        else
          @session.print e1020
        end
      elsif request.include? "GET" or request.include? "ALL"
        serve_loop nil,request
      else
        @session.print e1005
      end
    else
      @session.print e1000
    end
    @session.print m1010 if !request.nil?
    finish
  end
  private
  def serve_loop(user,req=nil)
    @session.print m1008
    if !req.nil? and req.include? "GET"
      retrieve req
    elsif !req.nil? and req.include? "ALL"
      retrieve_all
    else
      while !((request=@session.gets).nil?) and !request.include? "CLOSE"
        if request.include? "SET" and !user.nil?
          store user,request
        elsif request.include? "GET"
          retrieve request
        elsif request.include? "ALL"
          retrieve_all
        else
          @session.print e1000
        end
        @session.print m1011
      end
    end
  end
  def to_int(a)
    Integer a rescue false
  end
  def store(user,request)
    r=request.gsub(/SET /,'').split(',')
    if r.length!=2
      @session.print e1001
    elsif !r[0].is_a?(String)
      @session.print e1002
    elsif r[0]!=user
      @session.print e1007
    elsif !(rr=to_int(r[1]))
      @session.print e1003
    else
      set('soloists',['name','score'],[r[0],rr])
      @session.print m1100
    end
  end
  def retrieve(request)
    r=request.gsub(/GET /,'').chomp!
    if r.length==0
      @session.print e1001
    elsif !r.is_a?(String)
      @session.print e1002
    else
      if (rr=get('soloists','score',r))!=-1
        @session.print m1110+rr+"\n"
      else
        @session.print e1004
      end
    end
  end
  def retrieve_all
    scores=get_all("soloists")
    scores.each { |s_aux| @session.print s_aux[0]+","+s_aux[1]+"\n" }
  end
end

def quit?
  begin
    while c=STDIN.read_nonblock(2)
      return true if c==':q'
    end
    false
  rescue EOFError
    true
  rescue Exception
    false
  end
end

begin
  if ARGV[0].nil?
    worker=Server.new
  elsif ARGV[1].nil?
    worker=Server.new(ARGV[0])
  else
    worker=Server.new(ARGV[0],ARGV[1])
  end
rescue Errno::EACCES
  puts 'This port is reserved! Choose another one.'
  exit
end
puts 'Welcome stranger.',$welcome+'Server is now working.','Type :q or hit CTRL-D to exit.'
while not quit?
  i=worker.accept
  if i>0 then
    pid=fork do
      worker.serve
      worker.destroy
    end
    worker.finish
    Process.detach(pid)
  elsif i<0 then
    puts 'Server crashed! :('
    break
  end
end
worker.destroy
puts "\nThanks for using me. ;)"
