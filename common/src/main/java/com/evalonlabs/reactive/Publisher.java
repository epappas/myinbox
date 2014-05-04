package com.evalonlabs.reactive;

public interface Publisher<T> {
    public void subscribe(Subscriber<T> s);
}
