package com.saidake.plugin.generate.data;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;

@State(name = "DataHolder" ,storages = @Storage("plugin.xml"))
public class DataHolder implements PersistentStateComponent<DataState> {
    private DataState state = new DataState( );
    public static DataHolder getInstance() {
        return ServiceManager.getService(DataHolder.class);
    }

    @Override
    public DataState getState() {
        return state;
    }

    @Override
    public void loadState(DataState state) {
        this.state = state;
    }
}
