package com.saidake.plugin.generate.data.core;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import org.jetbrains.annotations.NotNull;


@State(name = "DataHolder" ,storages = @Storage("plugin.xml"))
public class DataHolder implements PersistentStateComponent<DataState> {

    private DataState state = new DataState();

    public static DataHolder getInstance() {
        return ServiceManager.getService(DataHolder.class);
    }

    @Override
    @NotNull
    public DataState getState() {
        return state;
    }

    @Override
    public void loadState(DataState state) {
        this.state = state;
    }
}
