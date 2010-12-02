open Unix
open List
open Str

class tcp=
object (self)
    val mutable client_sock=socket PF_INET SOCK_STREAM 0
    method private sock_send str=
        let len=String.length str in
        send client_sock str 0 len []
    method private sock_recv len=
        let str=String.create len in
        let recvlen=recv client_sock str 0 len [] in
        String.sub str 0 recvlen
    method private inet_parse results=
        let splitted=Str.split (Str.regexp "\n") results in
        let rec parse_aux input output=
            match input with
            |hd::tl->
                    let subbed=String.sub hd 0 1 in
                    if subbed="1"
                    then parse_aux tl output
                    else
                    (
                        let [name;score]=Str.split (Str.regexp ",") hd in
                        parse_aux tl ((name,int_of_string(score))::output)
                    )
            |_->stable_sort (fun (_,a) (_,b)->if a<b then 1 else if a>b then -1 else 0) output
        in parse_aux splitted []
    method sock_connect host port=
        try
            client_sock<-socket PF_INET SOCK_STREAM 0;
            connect client_sock (ADDR_INET(inet_addr_of_string(host),port));
            self#sock_recv 100;true
        with e->false
    method sock_close=
        self#sock_send "CLOSE";
        self#sock_recv 100;
        close client_sock
    method inet_register name pass=
        self#sock_send ("CREATE "^name^","^pass^"\n");
        let response=self#sock_recv 200 in
        self#sock_recv 100;
        String.sub response 0 4="1009"
    method inet_auth name pass=
        self#sock_send ("AUTH "^name^","^pass^"\n");
        let response=self#sock_recv 100 in
        String.sub response 0 4="1008"
    method inet_get_scores=
        self#sock_send "ALL\n";
        let msg=self#sock_recv 10000000 in
        self#inet_parse msg
    method inet_set_score name score=
        self#sock_send ("SET "^name^","^score^"\n");
        self#sock_recv 100
end
