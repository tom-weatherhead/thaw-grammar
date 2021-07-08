// cut-detector.ts

import { v4 as uuidv4 } from 'uuid';

export class CutDetector {
	public readonly guid: string;
	//public bool CutDetected { get; set; }

	constructor() {
		this.guid = uuidv4();
		//CutDetected = false;
	}
}
