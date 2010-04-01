#!/usr/bin/debugperl
#
# This is an attempt to work from a more top-down perspective on the
# Net::SILC::Client perl modules.  this script will strive to
# embody the typical usage of this low-level module in the manner of the
# "synopsis" given on most perl module doc pages.

# eventually, a new debugging script should support this one, illustrating
# the net-irc-like interface that is the next major step in development ;)

use strict;
use Devel::Peek;
use lib "lib/";
use Net::SILC;
use Net::SILC::Client;

my @user = ( username => "example", 
	  realname => "example user", 
	  hostname => "example.com",
	  keyfile => "example.pub"
  	   ); # example.prv is assumed

my $callbacks = { silc_say             => \&say,             # called when server says something
	          silc_channel_message => \&channel_message, # when channel message is received
	          silc_private_message => \&private_message, # when private message is received
	          silc_notify          => \&notify,         # called when server sends notify of server event
	          silc_command         => \&command,        # gets notice when commands are sent
	          silc_command_reply   => \&command_reply,  # gets notice when server replies
	          silc_connected       => \&connected,      # sent when server connects
	          silc_disconnected    => \&disconnected,   # sent when server disconnects
	          silc_failure         => \&failure,        # called whenever any server failure occurs

	          silc_key_agreement   => \&key_agreement,  # ignored (for now)
	          silc_ftp             => \&ftp,            # not implemented
	          silc_detach          => \&detach,         # not (yet) useful (to me)
             };

printf("creating new Client.pm object..\n");
#my $client = new Net::SILC::Client($callbacks,$user);
my $client = new Net::SILC::Client(@user);

my $server = "silc.icore.at";
printf("connecting to $server..\n");
$client->connect($server); # will cause the 'connected' callback to be triggered

#$client->add_handler(['connect'], \&on_connect);

# i realize increasingly that my focus on bots probly informs my
# decision about what's 'relevant' to include from the (extremely rich)
# silc library toolkit interface.  fortunately, this only serves to
# (further) motivate my desire for this project to be taken up by an
# "opensource community" of folks who, making various demands on this code,
# thereby ensure it's vitality and (appropriate) richness :)

# so the api for Net::IRC looks more like 'join', 'privmsg', 'nick', and
# possibly 'away', and 'schedule' are the key methods to provide, in
# addition to the connection creation and handler management interface

# to run forever:
#$client->run();

# do sumfin smart here, so i can send a signal here, and change the $run flag ;p

# to run once in a loop

# this doesn't work yet
#print "waiting for debugclient.pl to connect..\n";
#until ($client->state() > 0) { $client->run_once(); } # do nothing until the connected callback returns (thus chanigng the state)
#print "state changed!\n";
print "connected!\n";
while ($client->state() > 0) { $client->run_once(); }; # wait for further events

sub on_connect {
  print "debugclient's on_connect routine is called!\n";
}

# callback routines
# many of these will need a means to re-access the Net::SILC::Client object, so
# they can call methods on it.. (like 'command', 'join', etc)
sub say {            # server messages
  my($self,$msg) = @_; 
  print "Server Says: $msg\n"; 
}    

sub channel_message { # public messages
 my ($self,$nick,$channel,$msg,$fmsg,$action) = @_;

 print "$channel: $fmsg";
 $self->chanmsg($channel,"hi there, ".$nick);

 if ( ($channel =~ /#comms/) && ($msg =~ /get out/) ) {
   $self->command("LEAVE $channel");
 }
}   

sub private_message { # private messages
 my ($self,$nick,$msg,$fmsg) = @_;
 print "$fmsg";
 $self->privmsg($nick,"replying to yer ".$msg);
 $self->command("WHOIS $nick");
}

# these should ultimately translate into events..
sub notify {
  my ($silc) = shift;
  my ($type) = shift;
  if ($type =~ /NONE/) {
    print shift;
  
  } elsif ($type =~ /JOIN/) {
    my $nick = shift;
    my $channel = shift;
    printf("Server Notify: $nick has JOINed $channel\n");

  } elsif ($type =~ /INVITE/) {
    my $channel = shift;  my $nick = shift;
    printf("Server Notify: $nick has been INVITEd to $channel\n");

  } elsif ($type =~ /LEAVE/) {
    my $nick = shift; my $channel = shift;
    printf("Server Notify: $nick has LEFT $channel\n");

  } elsif ($type =~ /SIGNOFF/) {
    my $nick = shift; my $msg = shift;
    printf("Server Notify: $nick has SIGNEDOFF ($msg)\n");

  } elsif ($type =~ /TOPIC/) {
    my $changer = shift; my $topic = shift; my $channel = shift;
    printf("Server Notify: $changer has changed TOPIC of $channel to $topic\n");

  } elsif ($type =~ /NICK/) {
    my $old = shift; my $new = shift;
    printf("Server Notify: $old has changed their NICK to $new\n") unless (!$old && !$new);

  } elsif ($type =~ /CMODE/) {
    my $changer = shift; my $mode = shift; my $channel = shift;
    printf("Server Notify: $changer changed CMODE of $channel ($mode)\n");

  } elsif ($type =~ /CUMODE/) {
    my $changer = shift; my $mode = shift;
    my $nick = shift; my $channel = shift;
    printf("Server Notify: $changer changed CUMODE of $nick on $channel ($mode)\n");

    $silc->command("TOPIC $channel Wheee!");

  } elsif ($type =~ /UMODE/) {
    my $clientid = shift; my $mode = shift;
    printf("Server Notify: $clientid changed UMODE ($mode)\n");

  } elsif ($type =~ /MOTD/) {
    printf("Server Notify: got MOTD: %s\n", shift);
  
  } elsif ($type =~ /CHANNEL/) {
    printf("Server Notify: %s changed it's ID (safely ignored)\n", shift);

  } elsif ($type =~ /KICK/) {
    my $kicked = shift; my $msg = shift; 
    my $kicker = shift; my $channel = shift;
    printf("Server Notify: $kicked has been KICKed from $channel by $kicker ($msg)\n");

  } elsif ($type =~ /KILL/) {
    my $killed = shift; my $msg = shift;
    my $killer = shift; my $channel = shift;
    printf("Server Notify: $killed has been KILLed from $channel by $killer ($msg)\n");

  } elsif ($type =~ /WATCH/) {
    my $nick = shift; my $mode = shift;
    printf("Server Notify: watched client $nick ($mode)\n");

  } elsif ($type =~ /ERROR/) {
    printf("Server Notify: ERROR\n");
  } else { 
    my $msg = shift;
    printf("(uncaught)Server Notify: %s (%s)\n", $type,$msg);
  }
}
sub command {}
sub command_reply {
  my ($self,$type) = @_[0,1];
  printf("debugclient got command_reply: $type\n");
  if ($type =~ /NONE/) {
  
  } elsif ($type =~ /WHOIS/) {
    my ($nick,$user,$real,$chanlist,$umode,$idletime,$fingerprint) = @_[2..8];
    printf("debugclient got a reply from WHOIS command for $nick ($umode):\n");
    printf("Real Name: $real\n");
    printf("Channels: $chanlist\n");
    printf("Idle: $idletime\n");
    printf("Fingerprint: $fingerprint\n");
  } elsif ($type =~ /NICK/) {
    my ($nick) = @_[2];
    printf("debugclient successfully changed its NICK to $nick\n");
  } elsif ($type =~ /LIST/) {
    my ($channel, $topic, $count) = @_[2..4];
    printf("Channel LIST: $channel ($count users)\t$topic\n");
  } elsif ($type =~ /TOPIC/) {
    my ($channel, $topic) = @_[2,3];
    printf("debugclient changed TOPIC on $channel: $topic\n");
  } elsif ($type =~ /INVITE/) {
  } elsif ($type =~ /KILL/) {
  } elsif ($type =~ /INFO/) {
  } elsif ($type =~ /OPER/) {
    printf("debugclient.pl successfully became silcoper\n");
  } elsif ($type =~ /JOIN/) {
    my ($channel,$mode,$topic) = @_[2..4];
    printf("debugclient JOINed $channel ($mode)\n");
    printf("Topic on $channel is: $topic\n");
  } elsif ($type =~ /MOTD/) {
 
  } elsif ($type =~ /UMODE/) {
  } elsif ($type =~ /CMODE/) {
  } elsif ($type =~ /CUMODE/) {
  } elsif ($type =~ /KICK/) {
  } elsif ($type =~ /BAN/) {
  } elsif ($type =~ /SILCOPER/) {
  } elsif ($type =~ /LEAVE/) {
    my $channel = @_[2];
    printf("debugclient has LEFT $channel\n");
  } elsif ($type =~ /USERS/) {
  } # leaving a few out here, cuz they're likely not implemented (yet)
  else { printf("debugclient.pl got an unhandled command_reply event: $type\n"); } 
}

# this gets triggered once the underlying library handles the verify_public_key and get_auth_method pieces
sub connected {
 my ($self,$server) = @_;
 print "debugclient.pl is connected to the server ($server)!\n";

 $self->command("LIST");
 $self->join("#commstest");
 $self->command("JOIN #commsmanifesto");
 $self->nick("spidey");
} 

sub disconnected {}

# not really handling these either, really ;p
sub failure {}
sub key_agreement {}
sub ftp {}
sub detach {}

# abstracting these out for simplicity (for now)
#sub get_auth_method {}
#sub verify_public_key {}
#sub ask_passphrase {}
