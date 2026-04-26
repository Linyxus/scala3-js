import './styles/app.css';
import { mountApp } from './ui/App';

const root = document.getElementById('app');
if (!root) throw new Error('Missing #app root element');
mountApp(root);
