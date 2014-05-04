package com.evalonlabs.reactive;

public interface Subscription {
    public void request(int n);

    public void cancel();
}
