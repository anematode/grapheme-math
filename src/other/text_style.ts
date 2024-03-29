import {Color} from "./color.js"

export type TextStyle = {
    color: Color,
    shadowColor: Color,
    font: string,
    fontSize: 12,
    shadowRadius: 0,
    align: 'left' | 'center' | 'right',
    baseline: 'bottom' | 'middle' | 'top'
};