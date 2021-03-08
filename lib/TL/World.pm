package TL::World;
use strict;
use warnings;

use TL::CA::Life;

sub new {
    my($class, $params) = @_;
    my $where = {};
    my $self = bless {
        options => $params,
        world => TL::CA::Life->new($params->{init_grid} // undef),
        life => $params->{init_life} // [],
        where => $where,
        tick_count => 0,
        state => 0,
    }, $class;
    $where->{join ':', @{ $_->location }} = $_ for @{ $self->life };
    return $self;
}

sub world { $_[0]->{world} }
sub life { $_[0]->{life} }
sub where { $_[0]->{where} }
sub state { $_[0]->{state} }
sub metabolism { $_[0]->{options}{metabolism} // 4 }
sub step_count {
    my($self) = @_;
    return $self->{tick_count} * ($self->metabolism + 1) + $self->{state};
}

sub read_cell {
    my($self, $x, $y) = @_;
    return +(
        $self->world->read_cell($x, $y),
        $self->where->{"$x:$y"} // undef,
    );
}
        
sub live {
    my($self) = @_;
    # where there's life there's hope; no requirements on grid
    return @{ $self->life } ? 1 : 0;
}

sub life_tick {
    my($self) = @_;
    $self->parthenogenesis_tick;
    $self->move_tick;   # includes conflict resolution
    $self->action_tick;
    return;
}

sub tick {
    my($self) = @_;
    $self->life_tick for 1 .. $self->metabolism;
    $self->world->tick; # advance the CA
    return;
}

sub step {
    my($self, $step_size) = @_;
    my($state, $meta) = ($self->state, $self->metabolism);
    while ($step_size) {
        if ($state == 0 && $step_size > $meta + 1) {
            $self->tick;
            $step_size -= $meta + 1;
            ++$self->{tick_count};
        } elsif ($state == $meta) {
            $self->world->tick;
            --$step_size;
            $state = 0;
            ++$self->{tick_count};
        } else {
            $self->life_tick;
            --$step_size;
            ++$state;
        }
    }
    $self->{state} = $state;
    return;
}

sub report {
    my($self) = @_;
    $self->world->dump_tiles;
    $_->dump for @{ $self->life };
    return;
}

sub parthenogenesis_tick {
    my($self) = @_;
    my $orgs = $self->life;
    for my $org (@$orgs) {
        my $new = $org->spawn or next;
        push @$orgs, $new;
        # same location, so no need to update $self->where: the upcoming
        # move_tick will move them both out, to resolve the matter
    }
    return;
}

sub resolve_clash {
    my($self, $loc, $orgs) = @_;
    # for now, just kill all organisms that attempt to occupy the same space
    $_->kill for @$orgs;
    return undef;
}

sub move_tick {
    my($self) = @_;
    my(%where, %clash);
    for my $org (@{ $self->life }) {
        my $loc = join ':', @{ $org->move };
        if ($where{$loc}) {
            $clash{$loc} //= [ $where{$loc} ];
            push @{ $clash{$loc} }, $org;
        } else {
            $where{$loc} = $org;
        }
    }
    while (my($loc, $orgs) = each %clash) {
        delete $where{$loc};
        my $repl = $self->resolve_clash($loc, $orgs);
        $where{$loc} = $repl if $repl;
        my %kill = (map +("$_" => 1), @$orgs);
        delete $kill{"$repl"} if $repl;
        @{ $self->life } = grep !$kill{"$_"}, @{ $self->life };
    }
    $self->{where} = \%where;
    return;
}

sub action_tick {
    my($self) = @_;
    my $world = $self->world;
    my $orgs = $self->life;
    for my $i (reverse 0 .. $#$orgs) {
        my $org = $orgs->[$i];
        my $loc = $org->location;
        my $cur = $world->read_cell(@$loc);
        my $next = $org->action($cur);
        my $e = $org->energy + $cur - $next;
        if ($e < 0) {
            # Organism cannot go below 0: instead it dies, and the
            # requested action does not occur
            $org->kill;
            splice @$orgs, $i, 1;
            next;
        }
        $org->set_energy($e);
        $world->write_cell(@$loc, $next);
    }
}

1;
