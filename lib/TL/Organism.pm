package TL::Organism;
use strict;
use warnings;

my %dir = qw{ L -1 S 0 R 1 };
my %revdir = reverse %dir;
my @face_x = (1, 0, -1, 0);
my @face_y = (0, 1, 0, -1);

my $name = 'A';
sub new {
    my($proto, $hashref) = @_;
    return bless {
        %$hashref,
        dead => 0,
        state => 0,
        name => $name++,
    }, ref($proto) || $proto;
}

sub init {
    my($class, $location, $facing, $energy, $chance, $genome) = @_;
    return $class->new({
        location => [ @$location ],
        facing => $facing,
        energy => $energy,
        needed => $energy,
        spawn_chance => $chance,
        genome => _parse($genome),
    });
}

sub location { $_[0]->{location} }
sub facing { $_[0]->{facing} }
sub energy { $_[0]->{energy} }
sub set_energy { $_[0]->{energy} = $_[1] }
sub needed { $_[0]->{needed} }
sub spawn_chance { $_[0]->{spawn_chance} }
sub genome { $_[0]->{genome} }
sub state { $_[0]->{state} }
sub dead { $_[0]->{dead} }
sub kill { $_[0]->{dead} = 1 }

#
# Move a step in the direction we're facing, and return our new location.
#
sub move {
    my($self) = @_;
    my $loc = $self->{location};
    my $facing = $self->{facing};
    $loc->[0] += $face_x[$facing];
    $loc->[1] += $face_y[$facing];
    return $loc;
}

#
# Progress our program a step, given the current state of the cell we
# are on. Returns the new state the cell should take.
#
sub action {
    my($self, $cur) = @_;
    my($write, $direction, $state) = @{ $self->{genome}[$self->{state}][$cur] };
    $self->{facing} = ($self->{facing} + $direction) % 4;
    $self->{state} = $state;
    return $write;
}

#
# Attempt to divide; returns new child if successful, else undef.
#
sub spawn {
    my($self) = @_;
    my $e = $self->energy;
    $e -= @{ $self->genome };   # cost to copy
    my $needed = $self->needed;
    return undef unless $e >= $needed * 2;

    my $chance = $self->spawn_chance;
    return undef unless rand() <= $chance;

    my $child_e = $e >> 1;
    $self->set_energy($e - $child_e);
    return $self->new({
        location => [ @{ $self->location } ],
        facing => $self->facing ^ 2,  # facing the opposite direction
        energy => $child_e,
        needed => $needed,
        spawn_chance => $chance,
        genome => $self->copy_genome,
    });
}

sub copy_genome {
    my($self) = @_;
    my $genome = $self->genome;
    # TODO: mutation
    return [ map [ map [ @$_ ], @$_ ], @$genome ];
}

sub dump {
    my($self) = @_;
    printf "[%s,%s] %s %s: %s\n",
            @{ $self->{location} }, $self->{facing}, $self->{energy},
            _dump_prog(@$self{qw{ genome state }});
    return;
}

sub _parse {
    my($s) = @_;
    return [ map [
        map {
            my($write, $turn, $state) = m{^([01])([LSR])(\d+)$}
                    or die "Error parsing genome '$_'";
            [ $write, $dir{$turn}, $state ]
        } split /\s+/, $_
    ], split /\s*;\s*/, $s ];
}

sub _dump_prog {
    my($prog, $state) = @_;
    return join ' ', map {
        my $r = $prog->[$_];
        my $s = join ' ', map __PACKAGE__->rulestr($_), @{ $prog->[$_] };
        $_ == $state ? "<$s>" : $s;
    } 0 .. $#$prog;
}

sub rulestr {
    my($proto, $rule) = @_;
    return "$rule->[0]$revdir{$rule->[1]}$rule->[2]";
}

1;
