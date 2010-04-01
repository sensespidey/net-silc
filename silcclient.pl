#!/usr/bin/debugperl
#
# This is an attempt to make debugclient.pl obsolete, by incorporating it's
# translation from the text-based info Client.pm returns from it's C callbacks to
# silc-toolkit.  Eventually, this will be the main way to interact with
# Net::SILC.  (For those of you familiar with Net::IRC, this should look very
# familiar) 

use strict;
use Data::Dumper;
use Devel::Peek;
use lib "lib/";
use Net::SILC;
use Term::ReadKey;
ReadMode 'cbreak';

my $user = { username => "examplebot", 
             realname => "Example Bot", 
             hostname => "example.org",
	     server   => "silc.icore.at",
             keyfile => "mykey.pub", # mykey.prv is assumed
	     immed_connect => 1,
           }; 

print "creating Net::SILC factory..\n";
my $silc = new Net::SILC;

# this doesn't actually initiate a connection- just returns a Net::SILC::Client object 
print "getting a new connection..\n";
my $conn = $silc->newconn($user);  

# register at least a connection handler, so we'll get the connected event.
print "adding a handler..\n";
$conn->add_handler(['connected'],\&on_connect);
$conn->add_handler(['say'],\&on_say);
$conn->add_handler(['notify'], \&on_notify);
$conn->add_handler(['join'], \&on_join);
$conn->add_handler(['cumode'],\&on_cumode);
$conn->add_handler(['public'],\&on_public);
$conn->add_handler(['private'],\&on_private);
$conn->add_handler(['whois'], \&on_whois);
$conn->add_handler(['onmotd'], \&on_motd);

print "handlers added.\n";

print "going into main loop...\n";
while (1) {
  $silc->do_one_loop(); # this should loop over a whole buncha connections..
  #$conn->run_once(); # or $silc->do_one_loop? # this wont get the 'scheduled' event..
  sleep 1;		# or else we spin constantly, polling. lots of cpu.
}

sub on_motd {
  my ($self,$event) = @_;
  print "silcclient.pl motd event detected!\n";
  print "MOTD event:", Data::Dumper->Dump([\$event]);
}

sub on_connect {
  my ($self,$event) = @_;

print STDERR "EVENT: ";
$event->dump;

  print "silcclient.pl is connected!\n";
  $conn->command("join #bot");
}

sub on_join {
  my($self,$event) = @_;

print STDERR "EVENT: ";
$event->dump;

  my $chan = @{$event->to}[0];

  printf("silcclient.pl joined channel %s\n",$chan);
  $conn->chanmsg($chan,"silcclient ho!");
}

sub on_public {
  my($self,$event) = @_;

print STDERR "EVENT: ";
$event->dump;

  print "silcclient.pl received a public message:\n";
  printf("from: %s channel: %s msg: %s action: %s\n",$event->from,$event->to,$event->format,$event->args);

  my $who = $event->from;		# busted right now
  $conn->command("WHOIS $who");
}

sub on_private {
  my($self,$event) = @_;

print STDERR "EVENT: ";
$event->dump;

  print "silcclient.pl received a private message:\n";
  printf("from: %s msg: %s\n",$event->from,$event->to);
}

sub on_say {
  my $msg = shift;

  print "silcclient.pl got a 'say' event!\n";
  print "Server Says: $msg\n";
}

sub on_notify {
  my($self,$event) = shift;

print STDERR "EVENT: ";
$event->dump;

  print "silcclient.pl got a 'notify' event!\n";
  print "Notify: $event->from\n";
}

sub on_cumode {
  my ($self,$event) = @_;
  printf "silcclient.pl was notified of a cumode change: %s(%s) on %s
  (changed by %s)\n",$event->from,$event->to,$event->format,$event->args;
}

sub on_whois {
  my ($self,$event) = @_;
  my ($nick,$real,$chanlist,$umode,$idletime,$fingerprint);
  $nick = $event->from;
  $real = $event->to->[0];
  $umode = $event->format;
  ($chanlist,$idletime,$fingerprint) = $event->args;
  print "silcclient.pl(debug): got a reply from WHOIS command for $nick\n"; 
  print "Real Name: $real\n";
  print "Channels: $chanlist\n";
  print "Idle: $idletime\n";
  print "Fingerprint: $fingerprint\n";
}
