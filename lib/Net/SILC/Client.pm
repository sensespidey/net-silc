####################################################################
#                                                                  #
# Net::SILC -- Object-oriented Perl interface to a SILC server     #
#                                                                   #
#  Client.pm: The basic functions for a simple SILC Client           #
#                                                                     #
#								       #
#    Copyleft (c) 2004 Derek Laventure spiderman at tranzform dotte ca  #
#                       and Ken Chase math at sizone dotte org         #
#                       All rights reserved.                          #
#                                                                    #
#								    #
#      This module is free software; you can redistribute or       #
#      modify it under the terms of Perl's Artistic License.       #
#                                                                  #
####################################################################


# add testing pass (as well as general cleanup of checks/memory usage)
package Net::SILC::Client; # Test 1: use_ok

use Net::SILC::Event;
use Carp qw(cluck croak carp confess);
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;
#use Devel::Peek;
use strict;
use Inline C => 'DATA',   # Test 2: test for existence of silc-toolkit
           LIBS => '-L/usr/local/silc/lib -lsilc -lsilcclient -lpthread -ldl',
	   INC => '-I/usr/local/silc/include';

use vars (                # with a few exceptions...
        '$AUTOLOAD',    #   - the name of the sub in &AUTOLOAD
);

# The names of the methods to be handled by &AUTOLOAD.
my %autoloaded = ( 'nickname'  => undef,
                   'port'     => undef,
                   'username' => undef,
                   'verbose'  => undef,
                   'parent'   => undef,
                   'hostname' => undef,
                 );

#use sigtrap qw(handler  ?? to catch SIGINTs and make things die nicely?
# can't really do that here, mebbe..

my %_udef = (); # global handlers hashref

# Preloaded methods go here.

sub new {
   my $class = shift;

   # dummy callbacks so we can catch all the necessary silc-toolkit events
   my $callbacks = { 
      silc_say             => \&Net::SILC::Client::_cb_silc_say,             # called when server says something
      silc_channel_message => \&Net::SILC::Client::_cb_silc_channel_message, # when channel message is received
      silc_private_message => \&Net::SILC::Client::_cb_silc_private_message, # when private message is received
      silc_notify          => \&Net::SILC::Client::_cb_silc_notify,          # called when server sends notify of server event
      silc_command         => \&Net::SILC::Client::_cb_silc_command,         # gets notice when commands are sent
      silc_command_reply   => \&Net::SILC::Client::_cb_silc_command_reply,   # gets notice when server replies
      silc_connected       => \&Net::SILC::Client::_cb_silc_connected,       # sent when server connects
      silc_disconnected    => \&Net::SILC::Client::_cb_silc_disconnected,    # sent when server disconnects
      silc_failure         => \&Net::SILC::Client::_cb_silc_failure,         # called whenever any server failure occurs

      silc_key_agreement   => \&Net::SILC::Client::_cb_silc_key_agreement,   # ignored (for now)
      silc_ftp             => \&Net::SILC::Client::_cb_silc_ftp,             # not implemented
      silc_detach          => \&Net::SILC::Client::_cb_silc_detach,          # not (yet) useful (to me)
   };

   # what needs to happen is this:
   # we'll create a new ClientConn here, and then connect() will call
   # run_once on ourselves until the _cb_silc_connected callback fires
   # indicating our changed state.

   my $parent = shift;
   my $dbg = $parent->{_debug};

   my $self = {
     callbacks   => $callbacks,  # each of these should become a global handler

     # and then the callbacks hashref we actually pass contains references
     # to our internal _cb_silc_ routines, yes?

     _parent     => $parent,  # move this up eventually (?)
     user	 => shift,
     _server     => "",
     _handler    => {},
     _debug      => $dbg,
     #_debug      => $_[0]->{_debug},
     _connected  => 0
   };  # new hash for the client object (a SilcClient and some SilcClientConnections)

   bless($self, $class);

print STDERR "::Client:new(): Calling client_create...\n";

#print STDERR "::Client::new()::$self\n", Data::Dumper->Dump([\$self]);

   client_create($self); # create ClientConn struct, store it in $self

print STDERR "done.\n";

   return $self;  # Test 3: PROFIT!! ^W^W successful creation of client
}

# This sub is the common backend to add_handler and add_global_handler
#
sub _add_generic_handler
{
    my ($self, $event, $ref, $rp, $hash_ref, $real_name) = @_;
    my $ev;
    my %define = ( "replace" => 0, "before" => 1, "after" => 2 );

    unless (@_ >= 3) {
        croak "Not enough arguments to $real_name()";
    }
    unless (ref($ref) eq 'CODE') {
        croak "Second argument of $real_name isn't a coderef";
    }

    # Translate REPLACE, BEFORE and AFTER.
    if (not defined $rp) {
        $rp = 0;
    } elsif ($rp =~ /^\D/) {
        $rp = $define{lc $rp} || 0;
    }

    # loop through event name(s)
    foreach $ev (ref $event eq "ARRAY" ? @{$event} : $event) {

        # Translate numerics to names
        my $recvdev = $ev;		# keep this around for debug

        # cant just translate numerics, must translate IRC events to SILCness
        # see if there IS a translation available, if so use it. --math
        (defined Net::SILC::Event->trans($ev)) && 
           ($ev = Net::SILC::Event->trans($ev));

        unless ($ev) {
            carp "Unknown event type in $real_name: $recvdev";
            return;
        }

	# store the handler/priority in the appropriate hashref
        $hash_ref->{lc $ev} = [ $ref, $rp ];
    }

    return 1;
}

# This sub will assign a user's custom function to a particular event which
# might be received by any Connection object.
# Takes 3 args:  the event to modify, as either a string or numeric code
#                   If passed an arrayref, the array is assumed to contain
#                   all event names which you want to set this handler for.
#                a reference to the code to be executed for the event
#    (optional)  A value indicating whether the user's code should replace
#                the built-in handler, or be called with it. Possible values:
#                   0 - Replace the built-in handlers entirely. (the default)
#                   1 - Call this handler right before the default handler.
#                   2 - Call this handler right after the default handler.
# These can also be referred to by the #define-like strings in %define.
sub add_global_handler {
    my ($self, $event, $ref, $rp) = @_;
        return $self->_add_generic_handler($event, $ref, $rp,
                                           \%_udef, 'add_global_handler');
}

# This sub will assign a user's custom function to a particular event which
# this connection might receive.  Same args as above.
sub add_handler {
    my ($self, $event, $ref, $rp) = @_;
        return $self->_add_generic_handler($event, $ref, $rp,
                                           $self->{_handler}, 'add_handler');
}

# stolen directly from Net::SILC::Connection :)
sub AUTOLOAD {
    my $self = @_;  ## can't modify @_ for goto &name
    my $class = ref $self;  ## die here if !ref($self) ?
    my $meth;

    # -- #perl was here! --
    #  <Teratogen> absolute power corrupts absolutely, but it's a helluva lot
    #              of fun.
    #  <Teratogen> =)
    
    ($meth = $AUTOLOAD) =~ s/^.*:://;  ## strip fully qualified portion

    unless (exists $autoloaded{$meth}) {
        croak "No method called \"$meth\" for $class object.";
    }
    
    eval <<EOSub;
sub $meth {
    my \$self = shift;
        
    if (\@_) {
        my \$old = \$self->{"_$meth"};
        
        \$self->{"_$meth"} = shift;
        
        return \$old;
    }
    else {
        return \$self->{"_$meth"};
    }
}
EOSub
    
    # no reason to play this game every time
    goto &$meth;
}


# Schedules an event to be executed after some length of time.
# Takes at least 2 args:  the number of seconds to wait until it's executed
#                         a coderef to execute when time's up
# Any extra args are passed as arguments to the user's coderef.
sub schedule {
    my ($self, $time, $code) = splice @_, 0, 3;

    unless ($code) {
        croak 'Not enough arguments to Client->schedule()';
    }
    unless (ref $code eq 'CODE') {
        croak 'Second argument to schedule() isn\'t a coderef';
    }

    $time = time + int $time;
    $self->parent->queue($time, $code, $self, @_);
}

# what's this for? convenience, i guess.
sub connected {
  my $self = shift;
  return client_state($self);
}

sub parent {
  my $self = shift;

  if (@_) {
    my $old = $self->{_parent} = shift;
    $self->{_parent} = shift;
    return $old;
  } else { 
    return $self->{_parent};
  }
}

sub handler {
    my ($self, $event) = splice @_, 0, 2;

    unless (defined $event) {
        croak 'Too few arguments to Client->handler()';
    }

    # Get name of event.
    my $ev;
    if (ref $event) {
        $ev = $event->type;
    } elsif (defined $event) {
        $ev = $event;
        $event = Net::SILC::Event->new($event, '', '', '');
    } else {
        croak "Not enough arguments to handler()";
    }

    print "Net::SILC::Client(debug): Trying to handle event '$ev'.\n" if $self->{_debug};

    my $handler = undef;
    if (exists $self->{_handler}->{$ev}) {
        $handler = $self->{_handler}->{$ev};
    } elsif (exists $_udef{$ev}) {
        $handler = $_udef{$ev};
    } else {
        return $self->_default($event, @_);
    }

    my ($code, $rp) = @{$handler};

    # If we have args left, try to call the handler.
    if ($rp == 0) {                      # REPLACE
        &$code($self, $event, @_);
    } elsif ($rp == 1) {                 # BEFORE
        &$code($self, $event, @_);
        $self->_default($event, @_);
    } elsif ($rp == 2) {                 # AFTER
        $self->_default($event, @_);
        &$code($self, $event, @_);
    } else {
        confess "Bad parameter passed to handler(): rp=$rp";
    }

    warn "Handler for '$ev' called.\n" if $self->{_debug};

    return 1;
}

# The following are really just convenience methods

# handle the public key management for this client, and connect to the server given 
sub connect {
   my $self = shift;
   my $server = shift;		# get connection args a la IRC

   my $port = shift || 706;

   print "\n::Client::connect: server, port: $server, $port \n";

   # setup the client keypair
   my $pubkey = $self->{user}{keyfile} || "mykey.pub";
   (my $prvkey = $pubkey) =~ s/pub/prv/;
   get_key_pair($self,$pubkey,$prvkey);

   # initiate async connection
   client_connect($self,$server,$port);   # Test 4: connect_to_server

   # wait for _cb_silc_connected to get called
   until ($self->state > 0) { $self->run_once(); }

   # fire a 'connected' event, in case any global handlers for this event
   # have already been registered- note that this event will happen again
   # if a new handler is added for this object, in case that handler has
   # not yet been notified of our connected status.  this allows client
   # scripts to reliably add_(global)_handlers for 'connected' routines
   # that take care of setting nicks, joining channels and such, without
   # having to worry about timing.. (well, hopefully ;b)
#   my $ev = new Net::SILC::Event("connected", $server);
#   $self->handler($ev);

   # all that's bogus- there's no way this event should be fired here-
   # instead, we should register it's connected-ness in the
   # $self object, and then run and run_once should trigger this event when
   # they notice the 'connected_event' flag unset in $self (and then set
   # it, so this doesn't happen every time!)

   $self->{_server} = $server;
   $self->{_connected} = 1;
   $self->{_connected_event} = 0;
   $self->parent->addconn($self);
}

# simple wrapper around command
sub join {                                # Test 5: join a channel
  my ($self,$chan) = @_;
  $self->command("JOIN $chan");
  #$self->run_once;		# jam command on thru
				# dangerous?!? not connected yet near startup?
				# mozbot and others join chans real early.
				# can be dangerous to join when state = 0
				# leave the commands in queue for now.
}

# -- #bot was here! --
# *** mozbot (mozbot@cust312.cpe.net.cable.rogers.com) has quit ()
# *** mozbot (mozbot@cust312.cpe.net.cable.rogers.com) has joined #bot
# *** mozbot (mozbot@cust312.cpe.net.cable.rogers.com) has quit ()
# *** mozbot (mozbot@cust312.cpe.net.cable.rogers.com) has joined #bot
# *** mozbot (mozbot@cust312.cpe.net.cable.rogers.com) has quit ()
# *** mozbot (mozbot@cust312.cpe.net.cable.rogers.com) has joined #bot
# *** mozbot (mozbot@cust312.cpe.net.cable.rogers.com) has quit ()
# *** mozbot (mozbot@cust312.cpe.net.cable.rogers.com) has joined #bot
# < peter> are you testing?
# < mATh> yeah
# < mATh> why? :)
# < peter> just wondering what debug info you're gathering by the join/quits
# < mATh> "what is peter's threshold before commenting about join/quit msgs?"
# < peter> :P


# in case we get tired of people
sub part {
  my ($self,$chan) = @_;

  # irc has a silly syntax:
  # Net::SILC::Client:part(): leaving '#bot :I was told to leave by mATh. :-('
  # for eg. 
  # have to hack the msg out of that. dont think silc supports part msgs(?)
  #  --math
  my $actualChan = $chan;		# we'ez gonna mangle this one.
  $actualChan =~ s/^(#[^ ]+) \:.*$/$1/;
  print "Net::SILC::Client:part(): leaving '$actualChan'\n";
  $self->command("LEAVE $actualChan");
  $self->run_once;	# force it thru. assumes we'd part only when connected
}

sub leave {
  part(@_);
}

# Mrs Pummelhorst! I want to get down now....
sub quit {
  my ($self) = @_;
  $self->command("QUIT");		# does this work? :) --math
  #$self->run_once;		# jam cmd on thru.
				# dont, causes segv's in the silc toolkit!
}

# -- #bot was here! --
# *** mozbot (mozbot@cust312.cpe.net.cable.rogers.com) has joined #bot
# < mATh> hi
# < peter> hello
# *** mozbot (mozbot@cust312.cpe.net.cable.rogers.com) has quit ()
# < mATh> heh
# < mATh> just trying to get some data dumped for ::SILC debug
# < mATh> derek forgot to make something extra-irc-like, im just 
#         emulating code from irc in silc here ;)
# < peter> ya, I just figured that since the bot wasn't replying I
#         could do it instead ;P
# < mATh> im actually testing the bot here, not trying to mitigate 
#         lonelyness
# < mATh> but your efforts are touching!
# < peter> heh
# * mATh hugs the #bot community

# set or get nick
# simple wrapper around command -- not so simple no more! --math
sub nick {                                # Test 6: set nick ??
  my ($self,$nick) = @_;
  #cluck "** ::Client::nick(): \$nick is '$nick'\n";

  # need to do IRC-ish stuff here - if sub is called with no
  # 2nd arg, then just return the nick. if its called with 2nd arg,
  # set nick, jus like Net::IRC::Connection::nick(). --math

  #print "* Client::nick(): \n", Data::Dumper->Dump([\$self]);

  # set nick to $nick if provided, and do it on server too
  ($nick) && ($self->{_nick} = $nick) && 
    (return $self->command("NICK $nick"));

  # otherwise just return our nick as currently is thought to be
  return $self->{_nick};
}

sub whois {
  my ($self,$nick) = @_;
  $self->command("WHOIS $nick");
}

sub command {                             # Test 7: send a command to a server
  my($self,$cmd) = @_;                    # (provide dummy callback to catch it?)
  client_command($self,$cmd);
  #$self->run_once;		# push thru buffer, or it gets overwritten
				# and corrupt.
				# might be dangerous, not connected yet?
} 

sub chanmsg {                             # Test 8: send a message to a channel
  my($self,$channel,$msg) = @_;
  client_channel_message($self,$channel,$msg);
  $self->run_once;		# push msg thru buffer, or it gets overwritten
				# and corrupt
} 

sub privmsg {                             # Test 9: send a message to a person (ourself)
  # careful, IRC tends to send to everyone/channels as privmsg to #chan
  # need to test here for '#' in nick and send to chanmsg instead.

  my($self,$nick,$msg) = @_;

  #cluck "** ::Client::privmsg(): \msg to nick '$nick': '$msg'\n";

  # test if its really to a channel, typical thing for IRC scripts to do :p
  if ($nick =~ /^#./) {
    print STDERR "SILC::Client: translating IRC privmsg to #chan to chanmsg\n";
    chanmsg($self,$nick,$msg);
  } else {
    client_private_message($self,$nick,$msg);
    $self->run_once;		# push msg thru buffer, or it gets overwritten
				# and corrupt
  }
}

# convenience method
sub state { # what's the state of the SilcClientConnection?
  my($self) = shift;
  return client_state($self);
}

sub run { # loop infinitely, triggering callbacks as silc events arise
  my $self = shift; 

  if ($self->{_connected_event} == 0) {
    # fire connected event
    my $ev = new Net::SILC::Event("connected",$self->{_server},
       $self->{_server},'',$self->{_nick}); 
    $self->handler($ev);

    # set _connected_event to 1
    $self->{_connected_event} = 1;
  }
  client_run($self); 
  CORE::select (undef,undef,undef,0.1);	# 0.1s delay or we spin too hard on cpu
} 

sub run_once { # do one time step, and trigger any callbacks, then return
  my $self = shift;
  #printf("running once (%d): %s\n",$self->state(),$self->{user}->{username});
  # unless ($self->state() <= 0);
  if ($self->{_connected} && $self->{_connected_event} == 0) {
    # fire connected event
    my $ev = new Net::SILC::Event("connected",$self->{_server},$self->{_server},'',$self->{_nick}); 
    $self->handler($ev);
    # set _connected_event to 1
    $self->{_connected_event} = 1;
  }
  client_run_one($self);  # not sure how to call this yet
  CORE::select (undef,undef,undef,0.1);	# 0.1s delay or we spin too hard on cpu
} 

sub DESTROY {
  my $self = shift;
  client_destroy($self);
}

sub _default {
    my ($self, $event) = @_;
    my $verbose = $self->verbose;

    # Users should only see this if the programmer (me) fucked up.
    unless ($event) {
        croak "You EEEEEDIOT!!! Not enough args to _default()!";
    }

    # Reply to PING from server as quickly as possible.
    if ($event->type eq "ping") {
        $self->sl("PONG " . (CORE::join ' ', $event->args));

    } elsif ($event->type eq "disconnect") {

        # I violate OO tenets. (It's consensual, of course.)
#        unless (keys %{$self->parent->{_connhash}} > 0) {
#            die "No active connections left, exiting...\n";
#        }
    }

    printf("_default handler caught %s event.\n",$event->type);

    return 1;
}

# These are the internal callback routines to get info from the
# silc-toolkit and turn them into proper events..

# this one's timing is tricky: it gets called just after the first positive
# signs of connection are made, and it's safe to start joining channels..
# we must 'queue' a connected Event here, so the connect() routine above
# knows can hand back a connected object, and any client handlers will be
# properly triggered when they first call run/run_once..
sub _cb_silc_connected {
 my ($self,$server,$nick) = @_; # self is a Client.pm object
				# server is the hostname of the server (a string)
				# nick is the nickname we connected with

 # at this point, we simply want to 'queue' a connected event, so that the
 # first time the run loop goes, it will be fired.
 # for now, i'm gonna do this a hacky kinda way, and just register another
 # state var in $self:
 $self->{_connected_event} = 0;  # this indicates a connected event has not yet been fired to the client
 $self->{_server} = $server;
 $self->{_nick} = $nick;
} 

# here we can simply fire an event and pass the msg along
sub _cb_silc_say {            # server messages
  my($self,$msg) = @_; 
  printf "Net::SILC::Client(debug): Server Says: $msg\n" if $self->{_debug}; 

  $self->handler(Net::SILC::Event->new('say',$msg));
}    

sub _cb_silc_channel_message { # public messages -- and actions? --math
 my ($self,$nick,$channel,$msg,$fmsg,$action) = @_;

 print "Net::SILC::Client(debug): $channel: $fmsg" if $self->{_debug};

 # this should prolly be left to the client :) no backdoors here! --math
 #if ($msg =~ /get out/) {
 #  $self->command("LEAVE $channel");
 #}

 # not sure what ends up in $action for actions..? a 1 by looks of it?
 # why a 1? cant be array cast to scalar from @_... a flag? 1 = action?!
 # uncomment that if it gets fixed ;) --math
 #$self->handler(Net::SILC::Event->new('public',$nick,$channel,$msg,$action));
 $self->handler(Net::SILC::Event->new('public',$nick,$channel,$msg,$msg));
}   

sub _cb_silc_private_message { # private messages
 my ($self,$nick,$msg,$fmsg) = @_;
 print "Net::SILC::Client::_cb_silc_private_message: fmsg='$fmsg'";

 # backdoor commented out --math :)
 #if ($msg =~ /join (\#.*)/) {
 #  $self->command("join $1");
 #}

 # have to fill the args[] of the event too, 4th argument to Event::new...
 print "Net::SILC::Client::_cb_silc_private_message: msg='$msg'\n";
 $self->handler(Net::SILC::Event->new('private',$nick,$msg,'',$msg));
}

# these should ultimately translate into events..
sub _cb_silc_notify {
  my ($self) = shift;
  my ($type) = shift;
  my($ev,$channel,$nick,$msg);
  my($changer,$topic,$mode);
  if ($type =~ /NONE/) {
    my $msg = shift;
    printf "Net::SILC::Client: Server Notify: type NONE\n" if $self->{_debug};
    #print $msg;
    $ev = new Net::SILC::Event('notify',$msg);
  
  } elsif ($type =~ /JOIN/) {

    $nick = shift; $channel = shift;
    printf "Net::SILC::Client: Server Notify: $nick has JOINed $channel\n" if $self->{_debug};

    if ($nick eq $self->{_nick}) {	# is this the right test?
					# what if we login with nick X, change
					# to Y, someone else takes X and
					# joins to channel? is {_nick} current
					# nick, or inital connect nick? --math

      # channel is 5th array element ([4]) in event args
      $ev = new Net::SILC::Event('selfjoin',$nick,$channel,"","",$channel);
    } else {
      $ev = new Net::SILC::Event('join',$nick,$channel,"","",$channel);
    }

  } elsif ($type =~ /INVITE/) {
    $channel = shift;  $nick = shift;
    printf "Net::SILC::Client: Server Notify: $nick has been INVITEd to $channel\n" if $self->{_debug};
    $ev = new Net::SILC::Event('invite',$nick,$channel);

  } elsif ($type =~ /LEAVE/) {
    $nick = shift; $channel = shift;
    printf "Net::SILC::Client: Server Notify: $nick has LEFT $channel\n" if $self->{_debug};
    $ev = new Net::SILC::Event('leave',$nick,$channel);

  } elsif ($type =~ /SIGNOFF/) {
    $nick = shift; $msg = shift;
    printf "Net::SILC::Client: Server Notify: $nick has SIGNEDOFF ($msg)\n" if $self->{_debug};
    $ev = new Net::SILC::Event('signoff',$msg);

  } elsif ($type =~ /TOPIC/) {
    $changer = shift; $topic = shift; $channel = shift;
    printf "Net::SILC::Client: Server Notify: $changer has changed TOPIC of $channel to $topic\n" if $self->{_debug};
    $ev = new Net::SILC::Event('topic',$changer,$channel,$topic);

  } elsif ($type =~ /NICK/) {
    my $old = shift; my $new = shift;
    if ($self->{_debug}) {
     printf "Net::SILC::Client: Server Notify: $old has changed their NICK to $new\n" unless (!$old && !$new);
    }
    $ev = new Net::SILC::Event('nick',$old,$new);

  } elsif ($type =~ /CMODE/) {
    $changer = shift; $mode = shift; $channel = shift;
    printf "Net::SILC::Client: Server Notify: $changer changed CMODE of $channel ($mode)\n" if $self->{_debug};
    $ev = new Net::SILC::Event('cmode',$changer,$channel,$mode);

  } elsif ($type =~ /CUMODE/) {
    $changer = shift; $mode = shift;
    $nick = shift; $channel = shift;
    printf "Net::SILC::Client: Server Notify: $changer changed CUMODE of $nick on $channel ($mode)\n" if $self->{_debug};

    $ev = new Net::SILC::Event('cumode',$nick,$mode,$channel,$changer);

#    $self->command("TOPIC $channel Wheee!");

  } elsif ($type =~ /UMODE/) {
    my $clientid = shift; $mode = shift;
    printf "Net::SILC::Client: Server Notify: $clientid changed UMODE ($mode)\n" if $self->{_debug};

    $ev = new Net::SILC::Event('umode',$clientid,$mode);

  } elsif ($type =~ /MOTD/) {
    my $motd = shift;
    printf "Net::SILC::Client: Server Notify: got MOTD: %s\n", $motd if $self->{_debug};
    $ev = new Net::SILC::Event('motd',$motd);
  
  } elsif ($type =~ /CHANNEL/) {
    $channel = shift;
    printf "Net::SILC::Client: Server Notify: %s changed it's ID (safely ignored)\n", $channel if $self->{_debug};
    $ev = new Net::SILC::Event('channel',$channel);

  } elsif ($type =~ /KICK/) {
    my $kicked = shift; $msg = shift; 
    my $kicker = shift; $channel = shift;
    printf "Net::SILC::Client: Server Notify: $kicked has been KICKed from $channel by $kicker ($msg)\n" if $self->{_debug};
    $ev = new Net::SILC::Event('kick',$channel,$kicked,$kicker,$msg);

  } elsif ($type =~ /KILL/) {
    my $killed = shift; $msg = shift;
    my $killer = shift; $channel = shift;
    printf "Net::SILC::Client: Server Notify: $killed has been KILLed from $channel by $killer ($msg)\n" if $self->{_debug};
    $ev = new Net::SILC::Event('kill',$channel,$killed,$killer,$msg);

  } elsif ($type =~ /WATCH/) {
    $nick = shift; $mode = shift;
    printf "Net::SILC::Client: Server Notify: watched client $nick ($mode)\n" if $self->{_debug};
    $ev = new Net::SILC::Event('watch',$nick,$mode);

  } elsif ($type =~ /ERROR/) {
    printf "Net::SILC::Client: Server Notify: ERROR\n" if $self->{_debug};
    $ev = new Net::SILC::Event('error');
  } else { 
    $msg = shift;
    printf "Net::SILC::Client: (uncaught)Server Notify: %s (%s)\n", $type,$msg if $self->{_debug};
    #$ev = new Net::SILC::Event($type,$msg);
  }

  # fire the event
  $self->handler($ev) unless (!defined($ev));
}
sub _cb_silc_command {}
sub _cb_silc_command_reply {
  my ($self,$type) = @_[0,1];
  my $ev;

  # debug
  printf "Net::SILC::Client(debug): got command_reply: $type\n" if $self->{_debug};
  if ($type =~ /NONE/) {
  
  } elsif ($type =~ /WHOIS/) {
    my ($nick,$user,$real,$chanlist,$umode,$idletime,$fingerprint) = @_[2..8];
    $ev = new Net::SILC::Event('whois',$nick,$real,$umode,$chanlist,$idletime,$fingerprint); 
    if ($self->{_debug}) {
      printf "Net::SILC::Client(debug): got a reply from WHOIS command for $nick ($umode):\n";
      printf "Real Name: $real\n";
      printf "Channels: $chanlist\n";
      printf "Idle: $idletime\n";
      printf "Fingerprint: $fingerprint\n";
    }
  } elsif ($type =~ /NICK/) {
    my ($nick) = $_[2];
    printf "Net::SILC::Client successfully changed its NICK to $nick\n" if $self->{_debug};
  } elsif ($type =~ /LIST/) {
    my ($channel, $topic, $count) = @_[2..4];
    printf "Net::SILC::Client(debug): Channel LIST: $channel ($count users)\t$topic\n" if $self->{_debug} ;
  } elsif ($type =~ /TOPIC/) {
    my ($channel, $topic) = @_[2,3];
    printf "Net::SILC::Client(debug): changed TOPIC on $channel: $topic\n" if $self->{_debug} ;
  } elsif ($type =~ /INVITE/) {
  } elsif ($type =~ /KILL/) {
  } elsif ($type =~ /INFO/) {
  } elsif ($type =~ /OPER/) {
    printf "Net::SILC::Client(debug): successfully became silcoper\n" if $self->{_debug};
  } elsif ($type =~ /JOIN/) {
    my ($channel,$mode,$topic) = @_[2..4];
    printf "Net::SILC::Client(debug): JOINed $channel($mode) (Topic: $topic)\n" if $self->{_debug}; 
  } elsif ($type =~ /MOTD/) {
 
  } elsif ($type =~ /UMODE/) {
  } elsif ($type =~ /CMODE/) {
  } elsif ($type =~ /CUMODE/) {
  } elsif ($type =~ /KICK/) {
  } elsif ($type =~ /BAN/) {
  } elsif ($type =~ /SILCOPER/) {
  } elsif ($type =~ /LEAVE/) {
    my $channel = $_[2];
    printf "Net::SILC::Client(debug): has LEFT $channel\n" if $self->{_debug};
  } elsif ($type =~ /USERS/) {
  } # leaving a few out here, cuz they're likely not implemented (yet)
  else { printf "Net::SILC::Client(debug): got an unhandled command_reply event: $type\n" if $self->{_debug} ; } 

  # fire the event
  $self->handler($ev) unless (!defined($ev));
}

sub _cb_silc_disconnected {}

# not really handling these either, really ;p
sub _cb_silc_failure {}
sub _cb_silc_key_agreement {}
sub _cb_silc_ftp {}
sub _cb_silc_detach {}
1;
__DATA__
=head1 NAME

Net::SILC::Client - Perl implementation of silcclient library

=head1 SYNOPSIS

  use Net::SILC::Client;

  my $user = { username => "example", 
  	       realname => "example user", 
	       hostname => "example.org",
	       keyfile => "mykey.pub"
	      }; # mykey.prv is assumed

  my $callbacks = { silc_say             => \&say,
	            silc_channel_message => \&channel_message,
	            silc_private_message => \&private_message,
	            silc_notify          => \&notify,
	            silc_command         => \&command,
	            silc_command_reply   => \&command_reply,
	            silc_connected       => \&connected,
	            silc_disconnected    => \&disconnected,
	            silc_failure         => \&failure,
	            silc_key_agreement   => \&key_agreement,
	            silc_ftp             => \&ftp,
	            silc_detach          => \&detach,
                   };

  my $client = new Net::SILC::Client($user,$callbacks);

  $client->connect("silc.icore.at"); # will cause the 'connected' callback to be triggered

  ... (then in the appropriate callbacks, including but not before 'connected')
  
  $client->join("silc");
  $client->chanmsg("silc","hello");
  $client->privmsg("spidey","hey spidey");

    =head1 DESCRIPTION

    This is a fairly straightforward porting of the Silc Client library
    interface into the world of perl.  This is basically an initial step
    toward the development of a full Net::SILC library, which ideally will
    provide a drop-in replacement for Net::IRC-based scripts to use, as
    well as provide ready access to the full richness of the silc-toolkit
    libraries more generally. 

    =head2 EXPORT

    None by default (OO interface).

    =head1 SEE ALSO

    README and TODO files included with Net::SILC 
    http://silcnet.org/docs/toolkit/silcclientlib.html
    http://search.cpan.org/~jmuhlich/Net-IRC-0.75/IRC.pm

    If you have a mailing list set up for your module, mention it here.
    (none yet, perhaps soon)

    If you have a web site set up for your module, mention it here.
    http://silcpm.sourceforge.net/ (obsolete sf page for this stuff)

    =head1 AUTHOR

    Derek Laventure, E<lt>spiderman at tranzform dot caE<gt>

    =head1 COPYRIGHT AND LICENSE

    Copyright (C) 2004 by Derek Laventure

__C__

/* basic silc includes to get client library */
#include "silcincludes.h"
#include "silcclient.h"

/* The SilcClientOperations definition */
SilcClientOperations ops;

/* The following routines are used as helpers by the perl methods of this
 * class to setup and talk to the ClientConn struct stored in the self
 * hashref for Net::SILC::Client.  These are basically convenience methods,
 * which i believe could be bypassed already by calling the silc_client
 * functions directly (with appropriate args..)
 */

typedef struct {
  SilcClient client;
  SilcClientConnection conn;
  int state;
} *ClientConn;

/* create a new silc client connection */
SV* client_create(SV *self) {
   SV **user = NULL; /* for the user hashref, specifically */
   SV *sv_user = NULL;
   HV *hv_user = NULL;
   SV **svp_username = NULL; SV *sv_username = NULL; char *username = NULL;
   SV **svp_hostname = NULL; SV *sv_hostname = NULL; char *hostname = NULL;
   SV **svp_realname = NULL; SV *sv_realname = NULL; char *realname = NULL;
   HV *hv_self = NULL;
   SV *obj_ref = NULL;
   SV *obj = NULL;
   ClientConn c;  /* the client connection object to build */

   /* get the self object as a hash value, and then grab the user hash from that */
   hv_self = (HV*) SvRV(self);
   user = hv_fetch(hv_self,"user",4,FALSE);

   /* allocate a client connection */
   c = silc_calloc(1,sizeof(*c));
   if (!c) { perror("Out of memory"); return NULL; }

   /* allocate SilcClient, storing a SV* to reference value of 'self' for later retrieval */
   c->client = silc_client_alloc(&ops,NULL,(SV*)SvRV(self),NULL);
   if (!c->client) { perror("Could not allocate SILC Client"); return NULL; }

   /* setup sv_*name vars for initializing client */
   if (user == NULL) { 
     fprintf(stderr,"Net::SILC::Client: No user info provided to new().  Attempting to use defaults..\n"); 
     sv_username = (SV*) newSVpv(silc_get_username(),PL_na);
     sv_hostname = (SV*) newSVpv(silc_net_localhost(),PL_na);
     sv_realname = (SV*) newSVpv("Net::SILC::Client",17);

   } else {
     sv_user = (SV*) *user;
     hv_user = (HV*) SvRV(sv_user);

     /* grab the client paramaters */
     svp_username = hv_fetch(hv_user,"username",8,FALSE);
     svp_hostname = hv_fetch(hv_user,"hostname",8,FALSE);
     svp_realname = hv_fetch(hv_user,"realname",8,FALSE);

     if (svp_username != NULL) { sv_username = *svp_username; } 
     else {                     sv_username = (SV*) newSVpv(silc_get_username(),PL_na); }
     if (svp_hostname != NULL) { sv_hostname = *svp_hostname; } 
     else {                     sv_hostname = (SV*) newSVpv(silc_net_localhost(),PL_na); }
     if (svp_realname != NULL) { sv_realname = *svp_realname; } 
     else {                     sv_realname = (SV*) newSVpv(silc_get_real_name(),PL_na); }
   } 

   /* initialize *name vars for calling silc_client_init */
   username = SvPV_nolen(sv_username);
   hostname = SvPV_nolen(sv_hostname);
   realname = SvPV_nolen(sv_realname);

   /* debug */
   fprintf(stderr,"Net::SILC::Client: allocating a ClientConn object for %s (%s@%s)\n",username,realname,hostname);

   /* setup the SilcClient params */
   c->client->username = username; 
   c->client->hostname = hostname; 
   c->client->realname = realname;

   if (!silc_client_init(c->client)) { 
     perror("Net::SILC::Client: Could not initialize client"); 
     return; 
   }

   c->state = -1; /* not connected */

   /* construct an object reference to return */
   /* do i need to free sumfin here?? */
   obj_ref = sv_2mortal(newSViv(0));  /* create the pointer to the object */
   obj = newSVrv(obj_ref, NULL); /* create the object itself */
   sv_setiv(obj, (IV) c);   /* make the object point to the ClientConn object */

   SvREADONLY_on(obj);
   
   hv_store(hv_self,"client",6,SvREFCNT_inc(obj_ref),0);  /* store the pointer in self */
}

int client_destroy(SV *self) {
  HV *hv_self = NULL;
  SV *sv_client = NULL;
  SV **svp_client = NULL;
  ClientConn c; 

  hv_self = (HV*) SvRV(self);
  svp_client = hv_fetch(hv_self,"client",6,FALSE);

  /* debug */
  fprintf(stderr,"Net::SILC::Client: Deallocating ClientConn memory.\n");
  if (svp_client != NULL) {
    sv_client = (SV*) *svp_client;
    c = (ClientConn *) SvIV(SvRV(sv_client)); 

    /* call the disconnected callback */
    /* can't do this here cuz it's not declared yet */
    /* silc_disconnected(c,c->client,SILC_STATUS_ERR_TIMEDOUT,"Net::SILC::Client: Destroyed."); */

    /* call silc_client_free (and SilcClient), and then silc_free (on ClientConn) */
    silc_client_free(c->client);
    silc_free(c);
  } /* no else- i doesn't matter if the client isn't there anymore- nothin we can do */
}

/* load the keypair based on its two filenames, or create it if it doesn't already exist */
int get_key_pair(SV *self,char *pubkey,char *prvkey) {
  HV *hv_self = NULL;
  SV *sv_client = NULL;
  SV **svp_client = NULL;
  ClientConn c; 

  hv_self = (HV*) SvRV(self);
  svp_client = hv_fetch(hv_self,"client",6,FALSE);
  if (svp_client != NULL) {
    sv_client = (SV*) *svp_client;
    c = (ClientConn *) SvIV(SvRV(sv_client)); 
  } else { 
    fprintf(stderr,"Net::SILC::Client: get_key_pair called before client initialized.  Bad things will happen.\n");
    return;
  }

  /* debug */
  fprintf(stderr,"Net::SILC::Client: Initializing Key pair for %s (%s@%s)\n",c->client->realname,c->client->username,c->client->hostname); 
  if (!silc_load_key_pair(pubkey, prvkey, "", &c->client->pkcs,
                                              &c->client->public_key, 
					      &c->client->private_key) ) {
    /* no keys exist- generate them for the user */
   fprintf(stderr, "Net::SILC::Client: Key pair does not exist, generating it.\n");
   if (!silc_create_key_pair("rsa", 2048, pubkey, prvkey, NULL, "",
			      &c->client->pkcs,
			      &c->client->public_key,
			      &c->client->private_key, FALSE)) {
      fprintf(stderr,"Net::SILC::Client: Could not generate key pair (%s)\n",pubkey);
      return 1;
    }
  } else { fprintf(stderr,"Net::SILC::Client: Key pair loaded successfully. (%s)\n",pubkey); }
  return 0;
}

int client_connect(SV *self,char *server,int port) {
  HV *hv_self = NULL;
  SV **svp_client = NULL;
  ClientConn c;
  SilcClient s;

  hv_self = (HV*) SvRV(self);
  svp_client = hv_fetch(hv_self,"client",6,FALSE);
  if (svp_client != NULL) {
    c = (ClientConn *) SvIV(SvRV(*svp_client));
  } else {
    fprintf(stderr,"Net::SILC::Client: called client_connect before client initialized. Bad things will happen.\n");
    return;
  }

  /* debug */
  fprintf(stderr,"Net::SILC::Client: client_connect: %s (port %d)\n",server,port); 

  /* Start connecting to server.  This is asynchronous connecting so the
   * connection is actually created later after we run the client (via
   * silc_client_run or silc_client_run_once)
   */
  silc_client_connect_to_server(c->client, NULL, port, server, c);
}

/* accessor method to provide the state of the ClientConn stored in the Net::SILC::Client object */
int client_state(SV *self) {
  HV *hv_self = NULL;
  SV **svp_client = NULL;
  ClientConn c;

  hv_self = (HV*)SvRV(self);
  svp_client = hv_fetch(hv_self,"client",6,FALSE);

  if (svp_client != NULL) {
    c = (ClientConn *) SvIV(SvRV(*svp_client));
    return c->state;
  } else {
    return 0;
  }
}

void client_run(SV *self) {
  HV *hv_self = NULL;
  SV **svp_client = NULL;
  ClientConn c;

  hv_self = (HV*) SvRV(self);
  svp_client = hv_fetch(hv_self,"client",6,FALSE);
  if (svp_client != NULL) {
    c = (ClientConn *) SvIV(SvRV(*svp_client));
  } else {
    fprintf(stderr,"Net::SILC::Client: client_run called before client initialized. Bad things will happen.\n");
    return;
  }

  silc_client_run(c->client);
}

void client_run_one(SV *self) {
  HV *hv_self = NULL;
  SV **svp_client = NULL;
  ClientConn c;

  hv_self = (HV*) SvRV(self);
  svp_client = hv_fetch(hv_self,"client",6,FALSE);
  if (svp_client != NULL) {
    c = (ClientConn *) SvIV(SvRV(*svp_client));
  } else {
    fprintf(stderr,"Net::SILC::Client: client_run_one called before client initialized. Bad things will happen.\n");
    return;
  }

  silc_client_run_one(c->client);
}

int client_command(SV *self,char *cmd) {
  HV *hv_self = NULL;
  SV **svp_client = NULL;
  ClientConn c;

  hv_self = (HV*)SvRV(self);
  svp_client = hv_fetch(hv_self,"client",6,FALSE);
  if (svp_client != NULL) {
    c = (ClientConn *) SvIV(SvRV(*svp_client));
  } else {
    fprintf(stderr,"Net::SILC::Client: client_command called before client initialized. Bad things will happen.\n");
    return;
  }

  /* debug */
  fprintf(stderr,"Net::SILC::Client: sending command (%s) to server (%s)\n",cmd,c->conn->remote_host);
  
  /* careful here- this might not be initialized yet
   * TODO: implement a means to check if we're connected ;)
   */
  silc_client_command_call(c->client,c->conn,cmd);
}

int client_channel_message(SV *self, char *channel, char *msg) {
  HV *hv_self = (HV*)SvRV(self);
  SV **svp_client = NULL;
  ClientConn c;

  svp_client = hv_fetch(hv_self,"client",6,FALSE);
  if (svp_client == NULL) {
    fprintf(stderr,"Net::SILC::Client: client_channel_message called before client initialized. Bad things will happen.\n");
    return;
  } else {
    c = (ClientConn *) SvIV(SvRV(*svp_client));
  }

  /* debug */
  fprintf(stderr,"Net::SILC::Client: sending channel message (%s) to channel (%s)\n",msg,channel);

  silc_client_send_channel_message(c->client, c->conn,
  				   silc_client_get_channel(c->client,c->conn,channel),
				   NULL, SILC_MESSAGE_FLAG_NONE,
				   msg, strlen(msg),FALSE);
}

/* This is a simple callback used by the client_private_message method to
 * send the actual message passed to client_private_message.  i wish there
 * was a better implementation for this.. ;p
 */
static void on_private_message_client_lookup(SilcClient client, SilcClientConnection conn, SilcClientEntry *clients, SilcUInt32 clients_count, void *context) {

  char *msg = (char *)context;

  /* should probly do sumfin smarter than just use the first client entry returned ;p */
  silc_client_send_private_message(client,conn,clients[0],SILC_MESSAGE_FLAG_NONE,msg,strlen(msg),FALSE);
}

int client_private_message(SV *self, char *nick, char *msg) {
  HV *hv_self = (HV*)SvRV(self);
  SV **svp_client = NULL;
  ClientConn c;

  svp_client = hv_fetch(hv_self,"client",6,FALSE);
  if (svp_client != NULL) {
    c = (ClientConn *) SvIV(SvRV(*svp_client));
  }

  /* debug */
  fprintf(stderr,"Net::SILC::Client: sending private message (%s) to nickname %s\n",msg,nick);

  /* just do a simple search for the nick for now.. should properly account for multiple nicks later */
  silc_client_get_clients(c->client, c->conn, nick, c->conn->remote_host, on_private_message_client_lookup, msg);

}

/* These are helpers for the rest of the SilcClientOperations stuff */

/* Grab the named callback from the SILC::Client hashref */
SV* _get_callback(SV *self, char *cb) {
  SV **svp_callbacks = NULL;
  SV *sv_callbacks = NULL;
  SV **svp_cb = NULL;

  svp_callbacks = hv_fetch((HV*)self,"callbacks",9,FALSE); 
  if (svp_callbacks != NULL && SvROK((SV*)*svp_callbacks)) {
    sv_callbacks = (SV*) *svp_callbacks;
  } else {
    fprintf(stderr,"Net::SILC::Client: failed to retrieve callbacks hashref from SilcClient. Bad things will happen.(%s)\n",cb);
    return NULL;
  }

  /* grab the actual callback (as an SV*, but a pointer to CODE) */
  svp_cb = hv_fetch((HV*)SvRV(sv_callbacks),cb,strlen(cb),FALSE); 
  if (svp_cb != NULL && SvROK((SV*)*svp_cb)) {
    return (SV*) *svp_cb;
  } else {
    fprintf(stderr,"Net::SILC::Client: failed to retrieve callback from SilcClient. Bad things will happen. (%s)\n",cb);
    return NULL;
  }
}

/* this one takes a variable length arg list to handle the list of things to push on the arg stack */
/* Call the given callback (must be a CODEref) with the given list of args (and rv_self, too) */
/* actually want it to take an array of SV* values, so they can be pushed on the stack properly */
int _call_callback(SV *sv_cb, SV *rv_self, int num_args, SV **sv_list) { 
/* int _call_callback(SV *sv_cb, SV *rv_self, int num_args, ...) { */
  int i,result;
  SV arg;

  dSP;			/* initialize stack pointer */
  ENTER;		/* everything created after here */
  SAVETMPS;		/* ...is a temporary variable. */
  PUSHMARK(SP);		/* remember the stack pointer */
  XPUSHs(rv_self);	/* pass the original object (self) */

  /* now push the other args on here somehow */
  /* really need to pass a SV** (linked list) of these args.. */
  for (i=0 ; i < num_args ; i++) {
    XPUSHs(sv_2mortal(sv_list[i]));
  }

  PUTBACK;		/* make local stack pointer global */
  
  /* call the perl handler */
  result = perl_call_sv(sv_cb, G_SCALAR | G_EVAL);

  if (SvTRUE(ERRSV)) fprintf(stderr, "Net::SILC::Client: perl call errored: %s", SvPV(ERRSV,PL_na));
  SPAGAIN;		/* refresh the stack pointer */
  PUTBACK;		/* make local stack pointer global */
  FREETMPS;		/* free the return value */
  LEAVE;		/* .. and the XPUSHed args */

  return result;
}

/* convert a channel user mode to string */
char *cumode2str(unsigned int mode) {
  char *str_mode = strdup("");     /* setup dynamic string */

  /* generate mode string */

  if (mode & SILC_CHANNEL_UMODE_NONE) {
    str_mode = strdup("none");
  }

  if (mode & SILC_CHANNEL_UMODE_CHANFO) {
    str_mode = strcat(str_mode, "f");
  }

  /* CHANOP */
  if (mode & SILC_CHANNEL_UMODE_CHANOP) {
    str_mode = strcat(str_mode, "o");
  }

  if (mode & SILC_CHANNEL_UMODE_BLOCK_MESSAGES) {
    str_mode = strcat(str_mode, "b");
  }

  if (mode & SILC_CHANNEL_UMODE_BLOCK_MESSAGES_USERS) {
    str_mode = strcat(str_mode, "u");
  }

  if (mode & SILC_CHANNEL_UMODE_BLOCK_MESSAGES_ROBOTS) {
    str_mode = strcat(str_mode, "r");
  }

  if (mode & SILC_CHANNEL_UMODE_QUIET) {
    str_mode = strcat(str_mode, "q");
  }

  return str_mode;
}

/* convert a channel mode to string */
char *cmode2str(unsigned int mode) {
  char *str_mode = strdup("");     /* setup dynamic string */

  /* generate mode string */

  if (mode & SILC_CHANNEL_MODE_NONE) {
    str_mode = strdup("none");
  }

  if (mode & SILC_CHANNEL_MODE_PRIVATE) {
    str_mode = strcat(str_mode, "p");
  }

  if (mode & SILC_CHANNEL_MODE_SECRET) {
    str_mode = strcat(str_mode, "s");
  }

  if (mode & SILC_CHANNEL_MODE_PRIVKEY) {
    str_mode = strcat(str_mode, "k");
  }

  if (mode & SILC_CHANNEL_MODE_INVITE) {
    str_mode = strcat(str_mode, "i");
  }

  if (mode & SILC_CHANNEL_MODE_TOPIC) {
    str_mode = strcat(str_mode, "t");
  }

  if (mode & SILC_CHANNEL_MODE_ULIMIT) {
    str_mode = strcat(str_mode, "u");
  }

  if (mode & SILC_CHANNEL_MODE_FOUNDER_AUTH) {
    str_mode = strcat(str_mode, "f");
  }

  if (mode & SILC_CHANNEL_MODE_SILENCE_USERS) {
    str_mode = strcat(str_mode, "S");
  }

  if (mode & SILC_CHANNEL_MODE_SILENCE_OPERS) {
    str_mode = strcat(str_mode, "O");
  }

  if (mode & SILC_CHANNEL_MODE_PASSPHRASE) {
    /* guess you'd wanna grab the other arg that holds the passphrase ;p */
  }

  if (mode & SILC_CHANNEL_MODE_CIPHER) {
  }

  if (mode & SILC_CHANNEL_MODE_HMAC) {
  }

  if (mode & SILC_CHANNEL_MODE_CHANNEL_AUTH) {
  }

  return str_mode;
}

/* convert a user mode to string */
char *umode2str(unsigned int mode) {
  char *str_mode = strdup("");     /* setup dynamic string */

  /* generate mode string */

  if (mode & SILC_CHANNEL_MODE_NONE) {
    str_mode = strdup("none");
  }

  if (mode & SILC_UMODE_SERVER_OPERATOR) {
    str_mode = strcat(str_mode,"So");
  }
  
  if (mode & SILC_UMODE_ROUTER_OPERATOR) {
    str_mode = strcat(str_mode,"Ro");
  }
  
  if (mode & SILC_UMODE_GONE) {
    str_mode = strcat(str_mode,"g");
  }
  
  if (mode & SILC_UMODE_INDISPOSED) {
    str_mode = strcat(str_mode,"i");
  }
  
  if (mode & SILC_UMODE_BUSY) {
    str_mode = strcat(str_mode,"b");
  }
  
  if (mode & SILC_UMODE_PAGE) {
    str_mode = strcat(str_mode,"p");
  }
  
  if (mode & SILC_UMODE_HYPER) {
    str_mode = strcat(str_mode,"H");
  }
  
  if (mode & SILC_UMODE_ROBOT) {
    str_mode = strcat(str_mode,"B");
  }
  
  if (mode & SILC_UMODE_ANONYMOUS) {
    str_mode = strcat(str_mode,"a");
  }
  
  if (mode & SILC_UMODE_BLOCK_PRIVMSG) {
  }
  
  if (mode & SILC_UMODE_DETACHED) {
    str_mode = strcat(str_mode,"D");
  }
  
  if (mode & SILC_UMODE_REJECT_WATCHING) {
  }
  
  if (mode & SILC_UMODE_BLOCK_INVITE) {
  }
  
  return str_mode;
}
/*** SilcClientOperations callbacks ***/

static void silc_say(SilcClient client, SilcClientConnection conn, SilcClientMessageType type, char *msg, ...) {
  int result,args;
  char str[200];
  SV *sv_cb = NULL;             /* the callback itself */
  SV **sv_array = NULL;         
  
  /* reconstruct the Net::SILC::Client object carefully (into a blessed reference) */
  HV *stash = gv_stashpv("Net::SILC::Client",TRUE); /* get the stash for the Net::SILC::Client class */
  SV *self = (SV*)(client->application);  /* get the void* pointer fro SilcClient, and cast to SV* */
  SV *rv_self = newRV_noinc(self);        /* produce a reference to it */
  sv_bless(rv_self,stash);                /* and finally, bless it */

  /* construct the message from the variable arglist */
  va_list va;
  va_start(va, msg);
  vsnprintf(str, sizeof(str) - 1, msg, va);
  va_end(va);

  /* grab the callbacks hashref */
  sv_cb = _get_callback(self,"silc_say");

  /* setup the args */
  args = 1;
  sv_array = (SV**)malloc( sizeof (SV *) * args);
  sv_array[0] = newSVpvf("%s",str);

  result = _call_callback(sv_cb,rv_self,args,sv_array);
}

static void silc_channel_message(SilcClient client, SilcClientConnection conn,
                          SilcClientEntry sender, SilcChannelEntry channel,
                          SilcMessagePayload payload, SilcChannelPrivateKey key,
                          SilcMessageFlags flags, const unsigned char *message,
                          SilcUInt32 message_len) {
  int result,args;
  SV **sv_array = NULL;
  SV *sv_cb = NULL;             /* the callback itself */
  ClientConn c;
  
  /* reconstruct the Silc::Client object carefully (into a blessed reference) */
  HV *stash = gv_stashpv("Net::SILC::Client",TRUE); /* get the stash for the Net::SILC::Client class */
  SV *self = (SV*)(client->application);  /* get the void* pointer fro SilcClient, and cast to SV* */
  SV *rv_self = newRV_noinc(self);        /* produce a reference to it */
  sv_bless(rv_self,stash);                /* and finally, bless it */

  /* debug */
  fprintf(stderr, "Net::SILC::Client: received channel message\n"); 

  /* grab the callbacks hashref */
  sv_cb = _get_callback(self,"silc_channel_message");

  /* - nick of person who sent msg
   * - channel where msg came from
   * - actual message contents
   * - the formatted message
   * - action flag
   */
  if (flags & SILC_MESSAGE_FLAG_ACTION) {
    args = 5;
    sv_array = (SV**)malloc( sizeof (SV *) * args);
    sv_array[0] = newSVpvf("%s",sender->nickname);
    sv_array[1] = newSVpvf("%s",channel->channel_name);
    sv_array[2] = newSVpvf("%s",message);
    sv_array[3] = newSVpvf("%s %s\n", sender->nickname, message);
    sv_array[4] = newSViv(1);
  } else {
    args = 4;
    sv_array = (SV**)malloc( sizeof (SV *) * args);
    sv_array[0] = newSVpvf("%s",sender->nickname);
    sv_array[1] = newSVpvf("%s",channel->channel_name);
    sv_array[2] = newSVpvf("%s",message);
    sv_array[3] = newSVpvf("<%s> %s\n", sender->nickname, message);
  }

  result = _call_callback(sv_cb,rv_self,args,sv_array);
}

static void silc_private_message(SilcClient client, SilcClientConnection conn,
				 SilcClientEntry sender, SilcMessagePayload payload,
				 SilcMessageFlags flags,
				 const unsigned char *message,
				 SilcUInt32 message_len) {

  int result,args;
  SV **sv_array = NULL;
  SV *sv_cb = NULL;             /* the callback itself */
  
  /* reconstruct the Silc::Client object carefully (into a blessed reference) */
  HV *stash = gv_stashpv("Net::SILC::Client",TRUE); /* get the stash for the SILC::Client class */
  SV *self = (SV*)(client->application);  /* get the void* pointer fro SilcClient, and cast to SV* */
  SV *rv_self = newRV_noinc(self);        /* produce a reference to it */
  sv_bless(rv_self,stash);                /* and finally, bless it */

  /* debug */
  fprintf(stderr, "Net::SILC::Client: received private message\n");

  /* grab the callbacks hashref */
  sv_cb = _get_callback(self,"silc_private_message");

  /* - nick of person who sent msg
   * - actual message contents
   * - the formatted message
   */
  args = 3;
  sv_array = (SV**)malloc( sizeof (SV *) * args);
  sv_array[0] = newSVpvf("%s",sender->nickname);
  sv_array[1] = newSVpvf("%s",message);
  sv_array[2] = newSVpvf("PRV:<%s> %s\n", sender->nickname, message);

  result = _call_callback(sv_cb,rv_self,args,sv_array);
}

static void silc_notify(SilcClient client, SilcClientConnection conn, SilcNotifyType type, ...) {
  int result;
  int args = -1;
  SV **sv_array = NULL;
  SV *sv_cb = NULL;             /* the callback itself */
  
  char *str;
  SilcClientEntry client_entry,client_entry2;
  SilcChannelEntry channel_entry,channel_entry2;
  SilcServerEntry server_entry;
  SilcPublicKey public_key;
  SilcBuffer buffer;
  void *entry;
  SilcIdType idtype;
  SilcClientID client_id;
  SilcStatus status;
  unsigned int mode;
  char *changer,*str_mode;

  va_list va;
  va_start(va, type);

  /* reconstruct the Silc::Client object carefully (into a blessed reference) */
  HV *stash = gv_stashpv("Net::SILC::Client",TRUE); /* get the stash for the SILC::Client class */
  SV *self = (SV*)(client->application);  /* get the void* pointer to SilcClient, and cast to SV* */
  SV *rv_self = newRV_noinc(self);        /* produce a reference to it */
  sv_bless(rv_self,stash);                /* and finally, bless it */

  /* grab the callbacks hashref */
  sv_cb = _get_callback(self,"silc_notify");

  /* now handle the actual type of notify we've received.. */
  /* just setup the args and sv_array variables, and the _call_callback'll be called at the end.. */
  switch (type) {
  case SILC_NOTIFY_TYPE_NONE: /* Max Arguments:  1 */
    str = va_arg(va, char *);

    args = 2;
    sv_array = (SV**)malloc(sizeof(SV*) * args);
    
    sv_array[0] = newSVpvf("NONE");
    sv_array[1] = newSVpvf("--- %s\n", str); /* msg */
    break;

  /* notify that a client has been add|del from the invite list */
  case SILC_NOTIFY_TYPE_INVITE: /* Max Arguments:  5 */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(INVITE)\n");

    channel_entry = va_arg(va, SilcChannelEntry);  /* Channel */
    str = va_arg(va, char *);         		   /* channel_name */
    client_entry = va_arg(va, SilcClientEntry);    /* Inviter */

    args = 3; /* min args + 1 */
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("INVITE", str);
    sv_array[1] = newSVpvf("%s", str);  /* channel name */
    sv_array[2] = newSVpvf("%s", client_entry->nickname);  /* nick name */
    break;

  /* notify that a client has joined a channel (including this client) */
  case SILC_NOTIFY_TYPE_JOIN: /* Max Arguments:  2 */
    /* debug */
    /* fprintf(stderr,"Net::SILC::Client: NOTIFY(JOIN)\n"); */

    client_entry = va_arg(va, SilcClientEntry);
    channel_entry = va_arg(va, SilcChannelEntry);

    args = 3;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("JOIN");
    sv_array[1] = newSVpvf("%s", client_entry->nickname); /* ClientID */
    sv_array[2] = newSVpvf("%s", channel_entry->channel_name); /* ChannelID */
    break;

  /* notify that a client is leaving a channel (not including this client) */
  case SILC_NOTIFY_TYPE_LEAVE: /* Max Arguments: 1 */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(LEAVE)\n");

    client_entry = va_arg(va, SilcClientEntry);  /* leaving client */
    channel_entry = va_arg(va, SilcChannelEntry); 

    args = 3;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("LEAVE");
    sv_array[1] = newSVpvf("%s", client_entry->nickname); 
    sv_array[2] = newSVpvf("%s", channel_entry->channel_name); /* channel left */
    break;

  /* notify that a client is signing off the server (not including this client) */
  case SILC_NOTIFY_TYPE_SIGNOFF: /* Max Arguments: 2 */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(SIGNOFF)\n");

    client_entry = va_arg(va, SilcClientEntry);  /* signoff client */
    str = va_arg(va, char *); 			 /* signoff message */

    args = 3;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("SIGNOFF");
    sv_array[1] = newSVpvf("%s", client_entry->nickname); 
    sv_array[2] = newSVpvf("%s", str);  
    break;

  /* notify that a channel topic has been (re)set */
  case SILC_NOTIFY_TYPE_TOPIC_SET: /* Max Arguments: 2 */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(TOPIC_SET)\n");

    /* grab first two args */
    idtype = va_arg(va, int); 
    entry = va_arg(va, void *);
    str = va_arg(va, char *);  /* topic */
    channel_entry = va_arg(va, SilcChannelEntry);
    
    /* determine what type of entity set the topic */
    if (idtype & SILC_ID_SERVER) {
      server_entry = (SilcServerEntry)entry;
      changer = server_entry->server_name;
    } 
    if (idtype & SILC_ID_CHANNEL) {
      channel_entry2 = (SilcChannelEntry)entry;
      changer = channel_entry2->channel_name;
    }
    if (idtype & SILC_ID_CLIENT) {
      client_entry = (SilcClientEntry)entry;
      changer = client_entry->nickname;
    }

    args = 4;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("TOPIC");
    sv_array[1] = newSVpvf("%s", changer);
    sv_array[2] = newSVpvf("%s", str); /* topic */
    sv_array[3] = newSVpvf("%s", channel_entry->channel_name);
    
    break;
  /* notify that a client changed their nick- including this client */
  case SILC_NOTIFY_TYPE_NICK_CHANGE: /* Max Arguments: 3  */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(NICK)\n");

    client_entry = va_arg(va, SilcClientEntry);  /* old client */
    client_entry2 = va_arg(va, SilcClientEntry); /* New Client */

    args = 3;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("NICK");
    sv_array[1] = newSVpvf("%s", client_entry->nickname); 
    sv_array[2] = newSVpvf("%s", client_entry2->nickname); 
    break;

  /* notify that channel mode has changed - sent to channel */
  case SILC_NOTIFY_TYPE_CMODE_CHANGE: /* Max Arguments: 8 */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(CMODE)\n");

    idtype = va_arg(va, int);		/* type of changer */
    entry = va_arg(va, void *);		/* changer entry */

    mode = va_arg(va, int);		/* mode */
    str = va_arg(va, char *);           /* cipher name (ignored) */
    str = va_arg(va, char *);           /* hmac name (ignored) */
    str = va_arg(va, char *);		/* passpharse (ignored) */
    public_key = va_arg(va, SilcPublicKey); /* ignored */
    buffer = va_arg(va, SilcBuffer);    /* ignored */
    channel_entry = va_arg(va, SilcChannelEntry);  /* the channel */

    /* determine what type of entity set the topic */
    if (idtype & SILC_ID_SERVER) {
      server_entry = (SilcServerEntry)entry;
      changer = server_entry->server_name;
    } 
    if (idtype & SILC_ID_CHANNEL) {
      channel_entry2 = (SilcChannelEntry)entry;
      changer = channel_entry2->channel_name;
    }
    if (idtype & SILC_ID_CLIENT) {
      client_entry = (SilcClientEntry)entry;
      changer = client_entry->nickname;
    }

    /* convert mode string */
    str_mode = cmode2str(mode);

    args = 4;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("CMODE");
    sv_array[1] = newSVpvf("%s", changer); 			/* changer (client or server) */
    sv_array[2] = newSVpvf("%s", str_mode);    			/* mode mask */
    sv_array[3] = newSVpvf("%s", channel_entry->channel_name);  /* channel */
    break;

  /* notify that a user mode on a channel has changed */
  case SILC_NOTIFY_TYPE_CUMODE_CHANGE: /* Max Arguments: 4 */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(CUMODE)\n");

    idtype = va_arg(va, int);		          /* type of changer */
    entry = va_arg(va, void *);		          /* entry of changer */
    mode = va_arg(va, unsigned int);		  /* mode mask */
    client_entry = va_arg(va, SilcClientEntry);   /* target */
    channel_entry = va_arg(va, SilcChannelEntry); /* channel */

    /* identify changer */
    if (idtype == SILC_ID_CLIENT) {
      client_entry2 = (SilcClientEntry)entry;
      changer = client_entry2->nickname;
    }
    if (idtype == SILC_ID_SERVER) {
      server_entry = (SilcServerEntry)entry;
      changer = server_entry->server_name;
    }
    if (idtype == SILC_ID_CHANNEL) {
      channel_entry2 = (SilcChannelEntry)entry;
      changer =  channel_entry2->channel_name;
    }

    /* convert mode string */
    str_mode = cumode2str(mode);

    /* collect args to pass up */
    args = 5;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("CUMODE");
    sv_array[1] = newSVpvf("%s", changer); 			/* changer (client or server) */
    sv_array[2] = newSVpvf("%s", str_mode);    			/* mode mask */
    sv_array[3] = newSVpvf("%s", client_entry->nickname); 	/* target */
    sv_array[4] = newSVpvf("%s", channel_entry->channel_name);  /* channel */
    break;

  /* notify of message of the day */
  case SILC_NOTIFY_TYPE_MOTD: /* Max Arguments: 1 */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(MOTD)\n");
    args = 2;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("MOTD");
    str = va_arg(va, char *);
    sv_array[1] = newSVpvf("MOTD[%s]\n", str); /* MOTD */
    break;

  /* notify that ChannelID has changed */
  case SILC_NOTIFY_TYPE_CHANNEL_CHANGE: /* Max Arguments: 2 */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(CHANNEL_CHANGE)\n");
    args = 2;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("CHANNEL");
    channel_entry = va_arg(va, SilcChannelEntry);
    sv_array[1] = newSVpvf("%s", channel_entry->channel_name); /* Old ChannelID */
    break;

  /* notify that a server has signed off the network */
  case SILC_NOTIFY_TYPE_SERVER_SIGNOFF: /* Max Arguments: 256 */
    /* debug */
    /* fprintf(stderr,"Net::SILC::Client: NOTIFY(SERVER_SIGNOFF)\n");
    /* ignoring this one (for now at least) */
    break;

  /* notify that a client was kicked off a channel (including this client) */
  case SILC_NOTIFY_TYPE_KICKED: /* Max Arguments: 3 */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(KICK)\n");

    client_entry = va_arg(va, SilcClientEntry);  /* kicked */
    str = va_arg(va, char *);			 /* kick msg */
    client_entry2 = va_arg(va, SilcClientEntry); /* kicker */
    channel_entry = va_arg(va, SilcChannelEntry);/* channel */

    args = 5;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("KICK");
    sv_array[1] = newSVpvf("%s", client_entry->nickname); /* kicked */
    sv_array[2] = newSVpvf("%s", str); /* comment */
    sv_array[3] = newSVpvf("%s", client_entry2->nickname); /* kicker */
    sv_array[4] = newSVpvf("%s", channel_entry->channel_name); /* channel */
    break;

  /* notify that a client has been killed from the network */
  case SILC_NOTIFY_TYPE_KILLED: /* Max Arguments: 3 */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(KILL)\n");

    client_entry = va_arg(va, SilcClientEntry);   /* killed */
    str = va_arg(va, char *);			  /* msg */
    idtype = va_arg(va, int);			  /* type of killer */
    entry = va_arg(va, void *);			  /* killer entry */
    channel_entry = va_arg(va, SilcChannelEntry); /* channel */

    /* identify changer */
    if (idtype == SILC_ID_CLIENT) {
      client_entry2 = (SilcClientEntry)entry;
      changer = client_entry2->nickname;
    }
    if (idtype == SILC_ID_SERVER) {
      server_entry = (SilcServerEntry)entry;
      changer = server_entry->server_name;
    }
    if (idtype == SILC_ID_CHANNEL) {
      channel_entry2 = (SilcChannelEntry)entry;
      changer =  channel_entry2->channel_name;
    }

    args = 4;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("KILLED");
    sv_array[1] = newSVpvf("%s", client_entry->nickname); 
    sv_array[2] = newSVpvf("%s", str); 
    sv_array[3] = newSVpvf("%s", changer); 
    sv_array[4] = newSVpvf("%s", channel_entry->channel_name); 
    break;

  /* notify that a user mode has changed */
  case SILC_NOTIFY_TYPE_UMODE_CHANGE: /* Max Arguments: 2 */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(UMODE)\n");

    client_id = va_arg(va, SilcClientID);
    mode = va_arg(va, int);
    str_mode = umode2str(mode);

    args = 3;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("UMODE_CHANGE");
    sv_array[1] = newSVpvf("%s", silc_id_id2str((void *)&client_id,SILC_ID_CLIENT)); /* ClientID */
    sv_array[2] = newSVpvf("%s", str_mode); /* mode mask */
    break;

  /* notify that ban list has changed */
  case SILC_NOTIFY_TYPE_BAN: /* Max Arguments: 3 */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(BAN)\n");
    /* ignoring for now */
    break;

  /* notify that some error has occured */
  case SILC_NOTIFY_TYPE_ERROR: /* Max Arguments: 256  */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(ERROR)\n");
    status = va_arg(va, int);

    args = 2;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("ERROR");
    sv_array[1] = newSVpvf("%d", status);  /* Status Type */
    break;

  /* notify that some watched user has changed */
  case SILC_NOTIFY_TYPE_WATCH: /* Max Arguments: 4 */
    /* debug */
    fprintf(stderr,"Net::SILC::Client: NOTIFY(WATCH)\n");
    
    client_entry = va_arg(va, SilcClientEntry);   /* watched client */
    str = va_arg(va, char *);			  /* new nickname */
    mode = va_arg(va, int);			  /* user mode */
    str_mode = umode2str(mode);

    args = 4;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("WATCH");
    sv_array[1] = newSVpvf("%s", str);  /* nickname */
    sv_array[2] = newSVpvf("%s", str_mode);  /* user mode */
    break;
  default:
    /* ignore the rest */
    break;
  }

  va_end(va); 
  
  /* and let the perl callbacks know */
  if (args >= 0) { result = _call_callback(sv_cb,rv_self,args,sv_array); }

  /* or sumfin.. */
  free(sv_array);
}

static void silc_command(SilcClient client, SilcClientConnection conn,
			 SilcClientCommandContext cmd_context, bool success,
			 SilcCommand command, SilcStatus status) {

  if (status != SILC_STATUS_OK) {
    fprintf(stderr, "Net::SILC::Client: COMMAND %s problem: %s \n",
		silc_get_command_name(command),
		silc_get_status_message(status));

  }
  /* and let the perl callbacks know */
}

static void silc_command_reply(SilcClient client, SilcClientConnection conn,
				SilcCommandPayload cmd_payload, bool success,
				SilcCommand command, SilcStatus status, ...) {
  int result;
  int args = -1;
  SV **sv_array = NULL;
  SV *sv_cb = NULL;

  /* stolen WHOLESALE from silky source! edit down after all these are written ;) */
  SilcUInt32 mode=0, list_count=0, idletime, usermode;
  SilcClientID *stored_clientid, *client_id;
  SilcChannelEntry channel_entry=NULL, userchannel;
  SilcChannelID *channel_id;
  SilcClientEntry client_entry;
  char *username, *nickname, *realname, *mode_str, *chan_str, *topic;
  unsigned char *fingerprint;
  void *entry;
  
  SilcBuffer client_id_list, channels;
  SilcDList dlist;
  SilcHashTableList userhtl;

  va_list va;
  
  /* if error occurred in client library, print the error */
  if (status != SILC_STATUS_OK) {
/*     fprintf(stderr, "Net::SILC::Client: COMMAND REPLY %s problem: %s\n",
			silc_get_command_name(command),
			silc_get_status_message(status));
 */
    /* and mebbe send the status up to the perlscript?! ;p */
    /* these should really be integrated into the arg processing below */
  }
  
  va_start(va, status);

  /* reconstruct the Silc::Client object carefully (into a blessed reference) */
  HV *stash = gv_stashpv("Net::SILC::Client",TRUE); /* get the stash for the SILC::Client class */
  SV *self = (SV*)(client->application);  /* get the void* pointer to SilcClient, and cast to SV* */
  SV *rv_self = newRV_noinc(self);        /* produce a reference to it */
  sv_bless(rv_self,stash);                /* and finally, bless it */

  /* grab the callbacks hashref */
  sv_cb = _get_callback(self,"silc_command_reply");

  /* this switch processes the arguments returned by the command_reply function */
  /* it leaves 'args' and 'sv_array' setup to be processed by the callback lines following */
  switch (command) {
  case SILC_COMMAND_WHOIS:

    /* first get all the args we care about */
    (void)va_arg(va, SilcClientEntry); /* ignore cuz the rest of the args contain all we (currently) want */
    nickname = va_arg(va, char *);     /* nickname */
    username = va_arg(va, char *);     /* username */
    realname = va_arg(va, char *);     /* realname */

    channels = va_arg(va, SilcBuffer); /* channels  (may be NULL) */
    mode = va_arg(va, int);           /* user mode */
    idletime = va_arg(va, int);        /* idletime */

    fingerprint = va_arg(va, unsigned char *); /* fingerprint (may be NULL) */

    /* then process them a little */

    /* stringify the channels list somehow */
    chan_str = strdup("|");
    if (channels != NULL) { 
      dlist = silc_channel_payload_parse_list(channels->data,channels->len); 
 
      // Traverse the list from the beginning to the end
      silc_dlist_start(dlist);
      while ((entry = silc_dlist_get(dlist)) != SILC_LIST_END) {
	SilcUInt32 name_len;

	chan_str = strcat(chan_str,silc_channel_get_name(entry, &name_len));
        chan_str = strcat(chan_str,"|");
      }
    } else { chan_str = strdup(""); }

    /* convert the user mode */
    mode_str = umode2str(mode);

   fingerprint = silc_fingerprint(fingerprint, 20);

    /* finally, setup the array */
    args = 8;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("WHOIS");
    sv_array[1] = newSVpvf("%s",nickname);
    sv_array[2] = newSVpvf("%s",username);
    sv_array[3] = newSVpvf("%s",realname);
    sv_array[4] = newSVpvf("%s",chan_str);
    sv_array[5] = newSVpvf("%s",mode_str);
    sv_array[6] = newSVpvf("%d",idletime);
    sv_array[7] = newSVpvf("%s",fingerprint);
    silc_free(fingerprint);
    
    break;
  case SILC_COMMAND_WHOWAS:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("WHOWAS");

    break;
  case SILC_COMMAND_IDENTIFY:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("IDENTIFY");

    break;
  case SILC_COMMAND_NICK:

    client_entry = va_arg(va, SilcClientEntry);   /* local entry */
    nickname = va_arg(va, char *);                /* nickname */
    
    args = 2;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("NICK");
    sv_array[1] = newSVpvf("%s",nickname);

    break;
  case SILC_COMMAND_LIST:  /* this is a single channel, in response to LIST command */
    channel_entry = va_arg(va, SilcChannelEntry);
    (void)va_arg(va, char *);
    topic = va_arg(va, char *);
    list_count = va_arg(va, unsigned int);  /* number of users */

    args = 4;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("LIST");
    sv_array[1] = newSVpvf("%s",channel_entry->channel_name);
    sv_array[2] = newSVpvf("%s",topic);
    sv_array[3] = newSVpvf("%d",list_count);

    break;
  case SILC_COMMAND_TOPIC:

    channel_entry = va_arg(va, SilcChannelEntry);
    topic = va_arg(va, char *);

    args = 3;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("TOPIC");
    sv_array[1] = newSVpvf("%s",channel_entry->channel_name);
    sv_array[2] = newSVpvf("%s",topic);

    break;
  case SILC_COMMAND_INVITE:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("INVITE");

    break;
  case SILC_COMMAND_KILL:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("KILL");

    break;
  case SILC_COMMAND_INFO:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("INFO");

    break;
  case SILC_COMMAND_STATS:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("STATS");

    break;
  case SILC_COMMAND_PING:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("PING");

    break;
  case SILC_COMMAND_OPER:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);
    /* no args */
    sv_array[0] = newSVpvf("OPER");
    break;
  case SILC_COMMAND_JOIN:
    
     (void)va_arg(va, char *);
    channel_entry = va_arg(va, SilcChannelEntry);
    mode = va_arg(va, int);
     (void)va_arg(va, SilcUInt32);
     (void)va_arg(va, unsigned char *);
     (void)va_arg(va, unsigned char *);
     (void)va_arg(va, unsigned char *);
    topic = va_arg(va, char *);

    /* debug */
    fprintf(stderr, "Net::SILC::Client: Joined channel '%s'\n", channel_entry->channel_name);

    mode_str = cmode2str(mode);

    args = 4;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("JOIN");
    sv_array[1] = newSVpvf("%s",channel_entry->channel_name);
    sv_array[2] = newSVpvf("%s",mode_str);
    sv_array[3] = newSVpvf("%s",topic);
    break;
  case SILC_COMMAND_MOTD:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("MOTD");

    break;
  case SILC_COMMAND_UMODE:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("UMODE");

    break;
  case SILC_COMMAND_CMODE:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("CMODE");

    break;
  case SILC_COMMAND_CUMODE:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("CUMODE");

    break;
  case SILC_COMMAND_KICK:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("KICK");

    break;
  case SILC_COMMAND_BAN:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("BAN");

    break;
  case SILC_COMMAND_DETACH:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("DETACH");

    break;
  case SILC_COMMAND_WATCH:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("WATCH");
    break;
  case SILC_COMMAND_SILCOPER:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("SILCOPER");

    break;
  case SILC_COMMAND_LEAVE:
    channel_entry = va_arg(va, SilcChannelEntry);

    args = 2;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("LEAVE");
    sv_array[1] = newSVpvf("%s",channel_entry->channel_name);

    break;
  case SILC_COMMAND_USERS:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("USERS");

    break;
  case SILC_COMMAND_GETKEY:
    args = 1;
    sv_array = (SV**)malloc(sizeof(SV*) * args);

    sv_array[0] = newSVpvf("GETKEY");
    break;
   default:
     fprintf(stderr, "Net::SILC::Client: Uncaught command_reply (%d)\n",command);
  }

  va_end(va);

  /* now let the (perl) command_reply callbacks know.. */
  if (args >= 0) { result = _call_callback(sv_cb,rv_self,args,sv_array); }

  /* or sumfin.. */
  free(sv_array);
}


/* this gets called by the silc-toolkit libs upon connection to the silc server
 * it means the connection process is complete (it's safe to call
 * command_call stuff on the object)- this can only happen after the
 * toolkit has run thru silc_client_connect_to_server to iniate the async
 * connection process *and* allowed the silc_schedule loop to run thru enuf
 * imes to trigger a low-level 'connected' event to happen in the toolkit.
 *
 * this here's the first time Client.pm receives the
 * SilcClientConnection object, so it's gonna save it somewhere, and change
 * it's internal state to reflect that.
 */
static void silc_connected(SilcClient client, SilcClientConnection conn, SilcClientConnectionStatus status) {
  int result,args;
  SV **sv_array = NULL;
  SV **svp_client = NULL;
  SV *sv_client = NULL;         /* the Net::SILC::Client object */
  SV *sv_cb = NULL;             /* the callback itself */
  ClientConn c;

  /* reconstruct the Silc::Client object carefully (into a blessed reference) */
  HV *stash = gv_stashpv("Net::SILC::Client",TRUE); /* get the stash for the SILC::Client class */
  SV *self = (SV*)(client->application);  /* get void* pointer from SilcClient, and cast to SV* */
  SV *rv_self = newRV_noinc(self);        /* produce a reference to self hashref */
  sv_bless(rv_self,stash);                /* and finally, bless it */

  /* grab the ClientConn object from Net::SILC::Client */
  svp_client = hv_fetch((HV*)self,"client",6,FALSE); 
  if (svp_client != NULL && SvROK((SV*)*svp_client)) {
    sv_client = (SV*) *svp_client;
    c = (ClientConn *) SvIV(SvRV(sv_client));
  } else {
    fprintf(stderr,"Net::SILC::Client: failed to retrieve self pointer from SilcClient. Bad things will happen.\n");
    return;
  }

  /* grab the callbacks hashref */
  sv_cb = _get_callback(self,"silc_connected");

  if (status == SILC_CLIENT_CONN_ERROR) {
    fprintf(stderr, "Net::SILC::Client: Could not connect to server\n");
    silc_client_close_connection(client, conn);
    return;
  }

  /* debug */
  fprintf(stderr, "Net::SILC::Client: Connected to server (%s).\n",conn->remote_host);

  /* save the connection context */
  c->state = 1; /* connected */
  c->conn = conn;

  /* and let the perl callbacks know we are connected */
  args = 2;
  sv_array = (SV**)malloc(sizeof(SV*) * args);
  sv_array[0] = newSVpvf("%s",conn->remote_host);
  sv_array[1] = newSVpvf("%s",conn->nickname);

  result = _call_callback(sv_cb,rv_self,args,sv_array);
}

static void silc_disconnected(SilcClient client, SilcClientConnection conn,
				SilcStatus status, const char *message) {
  int result,args;
  SV **sv_array = NULL;
  SV **svp_client = NULL;
  SV *sv_client = NULL;         /* the Net::SILC::Client object */
  SV *sv_cb = NULL;             /* the callback itself */
  ClientConn c;

  /* reconstruct the Silc::Client object carefully (into a blessed reference) */
  HV *stash = gv_stashpv("Net::SILC::Client",TRUE); /* get the stash for the SILC::Client class */
  SV *self = (SV*)(client->application);  /* get void* pointer from SilcClient, and cast to SV* */
  SV *rv_self = newRV_noinc(self);        /* produce a reference to self hashref */
  sv_bless(rv_self,stash);                /* and finally, bless it */

  /* grab the ClientConn object from Net::SILC::Client */
  sv_cb = _get_callback(self,"silc_disconnected");

  /* debug */
  fprintf(stderr, "Net::SILC::Client: Disconnected from server (%s).\n",conn->remote_host);
  fprintf(stdout, "Net::SILC::Client: %s:%s\n", silc_get_status_message(status), message);

  /* save the connection context */
  c->state = 0; /* disconnected */
  c->conn = NULL;

  /* and let the perl callbacks know we are disconnected */
  args = 1;
  sv_array = (SV**)malloc(sizeof(SV*) * args);
  sv_array[0] = newSVpvf("%s",conn->remote_host);

  result = _call_callback(sv_cb,rv_self,args,sv_array);
}

static void silc_get_auth_method(SilcClient client, SilcClientConnection conn,
				 char *hostname, SilcUInt16 port, SilcGetAuthMeth completion,
				 void *context) {

  /* we assume no auth is required in the server, and send nothing, triggering the final 'connected' signal */
  fprintf(stderr,"Net::SILC::Client: no authentication methods available.\n");
  completion(TRUE, SILC_AUTH_NONE, NULL, 0, context);

  /* and (for now, at least) dont bother with a perl callback for this */
}

static void silc_verify_public_key(SilcClient client, SilcClientConnection conn,
				   SilcSocketType conn_type, unsigned char *pk,
				   SilcUInt32 pk_len, SilcSKEPKType pk_type,
				   SilcVerifyPublicKey completion, void *context) {
   /* we accept the server's public key no matter what, thus triggering the 'get_auth_method' signal */
   fprintf(stderr,"Net::SILC::Client: auto-verifying public key\n");
   completion(TRUE, context);
   /* and for simplicity, dont provide a callback for this, just let it fall thru to the 'connected' signal */
}

static void silc_ask_passphrase(SilcClient client, SilcClientConnection conn,
				SilcAskPassphrase completion, void *context) {
  /* we dont support this at all (yet), so just return nothing */
  fprintf(stderr,"Net::SILC::Client: ask_passphrase not supported. skipping.\n");
  completion(NULL, 0, context);
}

static void silc_failure(SilcClient client, SilcClientConnection conn, SilcProtocol protocol, void *failure) {
  /* something bad must have happened during connecting to arrive here */
  fprintf(stderr, "Net::SILC::Client: Connecting failed (protocol failure)\n");
}

static bool silc_key_agreement(SilcClient client, SilcClientConnection conn,
			       SilcClientEntry client_entry, const char *hostname,
			       SilcUInt16 port, SilcKeyAgreementCallback *completion,
			       void **context) {
  return FALSE; /* no support for this either */
}

static void silc_ftp(SilcClient client, SilcClientConnection conn, SilcClientEntry client_entry,
		     SilcUInt32 session_id, const char *hostname, SilcUInt16 port) {
  /* no ftp either */
}

static void silc_detach(SilcClient client, SilcClientConnection conn,
			const unsigned char *detach_data, SilcUInt32 detach_data_len) {
  /* no silc detach, either (use screen!) */
}

/* The SilcClientOperation structure containing the operation functions.
   You will give this as an argument to silc_client_alloc function. */
SilcClientOperations ops = {
  silc_say,
  silc_channel_message,
  silc_private_message,
  silc_notify,
  silc_command,
  silc_command_reply,
  silc_connected,
  silc_disconnected,
  silc_get_auth_method,
  silc_verify_public_key,
  silc_ask_passphrase,
  silc_failure,
  silc_key_agreement,
  silc_ftp,
  silc_detach
};

/** Utility Functions **/

/* this one's stolen from the Inline::C cookbook */
/* http://search.cpan.org/~ingy/Inline-0.44/C/C-Cookbook.pod#Complex_Data */
void dump_hash(SV* hash_ref) {
   HV* hash;
   HE* hash_entry;
   int num_keys, i;
   SV* sv_key;
   SV* sv_val;

   if (! SvROK(hash_ref))
       croak("hash_ref is not a reference");

   hash = (HV*)SvRV(hash_ref);
   num_keys = hv_iterinit(hash);
   for (i = 0; i < num_keys; i++) {
       hash_entry = hv_iternext(hash);
       sv_key = hv_iterkeysv(hash_entry);
       sv_val = hv_iterval(hash, hash_entry);
       printf("%s => %s\n", SvPV(sv_key, PL_na), SvPV(sv_val, PL_na));
   }
   return;
}
