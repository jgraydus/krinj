import styled from 'styled-components'

const Button = styled.button`
    height: 28px !important;
    padding: 5px;
    border-radius: 6px;
    border-style: solid;
    border-width: 1px;
    user-select: none;

    &:enabled {
        border-color: black;
        background-color: #BFCFCF
        color: #DDD !important;
        cursor: pointer;
        &:focus { outline: thin dashed white; }
        &:hover { background-color: #B6C6C6; }
        &:active { background-color: #B0C0C0; }
    }

    &:disabled {
        border-color: #444;
        background-color: #BFCFCF;
        color: #444 !important;
        cursor: not-allowed;
        &:hover { background-color: #B6C6C6; }
    }
`

export default Button;

