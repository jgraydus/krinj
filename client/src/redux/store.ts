import { AnyAction, applyMiddleware, createStore } from 'redux';
import thunk from 'redux-thunk'
import api from '../api'
import { debugLogReducer } from './reducer'

export const store = createStore(
  debugLogReducer,
  applyMiddleware(thunk.withExtraArgument(api))
);

