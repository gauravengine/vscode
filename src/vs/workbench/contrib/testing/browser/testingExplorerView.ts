/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

import * as dom from 'vs/base/browser/dom';
import { ActionBar } from 'vs/base/browser/ui/actionbar/actionbar';
import { IIdentityProvider, IKeyboardNavigationLabelProvider, IListVirtualDelegate } from 'vs/base/browser/ui/list/list';
import { IListAccessibilityProvider } from 'vs/base/browser/ui/list/listWidget';
import { ObjectTree } from 'vs/base/browser/ui/tree/objectTree';
import { ITreeElement, ITreeEvent, ITreeFilter, ITreeNode, ITreeRenderer, ITreeSorter, TreeFilterResult, TreeVisibility } from 'vs/base/browser/ui/tree/tree';
import { findFirstInSorted } from 'vs/base/common/arrays';
import { throttle } from 'vs/base/common/decorators';
import { Emitter, Event } from 'vs/base/common/event';
import { FuzzyScore } from 'vs/base/common/filters';
import { Iterable } from 'vs/base/common/iterator';
import { Disposable, DisposableStore, IDisposable, toDisposable } from 'vs/base/common/lifecycle';
import { URI } from 'vs/base/common/uri';
import 'vs/css!./media/testing';
import { ICodeEditor } from 'vs/editor/browser/editorBrowser';
import { ICodeEditorService } from 'vs/editor/browser/services/codeEditorService';
import { Position } from 'vs/editor/common/core/position';
import { Location as ModeLocation } from 'vs/editor/common/modes';
import { localize } from 'vs/nls';
import { MenuEntryActionViewItem } from 'vs/platform/actions/browser/menuEntryActionViewItem';
import { MenuItemAction } from 'vs/platform/actions/common/actions';
import { IConfigurationService } from 'vs/platform/configuration/common/configuration';
import { IContextKeyService } from 'vs/platform/contextkey/common/contextkey';
import { IContextMenuService } from 'vs/platform/contextview/browser/contextView';
import { ITextEditorSelection } from 'vs/platform/editor/common/editor';
import { FileKind } from 'vs/platform/files/common/files';
import { IInstantiationService } from 'vs/platform/instantiation/common/instantiation';
import { IKeybindingService } from 'vs/platform/keybinding/common/keybinding';
import { WorkbenchObjectTree } from 'vs/platform/list/browser/listService';
import { IOpenerService } from 'vs/platform/opener/common/opener';
import { IProgressService } from 'vs/platform/progress/common/progress';
import { IStorageService, StorageScope, StorageTarget } from 'vs/platform/storage/common/storage';
import { ITelemetryService } from 'vs/platform/telemetry/common/telemetry';
import { IThemeService, ThemeIcon } from 'vs/platform/theme/common/themeService';
import { IWorkspaceFolder, IWorkspaceFoldersChangeEvent } from 'vs/platform/workspace/common/workspace';
import { ExtHostTestingResource } from 'vs/workbench/api/common/extHost.protocol';
import { TestRunState } from 'vs/workbench/api/common/extHostTypes';
import { IResourceLabel, IResourceLabelOptions, IResourceLabelProps, ResourceLabels } from 'vs/workbench/browser/labels';
import { ViewPane } from 'vs/workbench/browser/parts/views/viewPane';
import { IViewletViewOptions } from 'vs/workbench/browser/parts/views/viewsViewlet';
import { IViewDescriptorService } from 'vs/workbench/common/views';
import { testingStatesToIcons } from 'vs/workbench/contrib/testing/browser/icons';
import { cmpPriority, maxPriority, statePriority, statesInOrder } from 'vs/workbench/contrib/testing/browser/testExplorerTree';
import { ITestingCollectionService, TestSubscriptionListener } from 'vs/workbench/contrib/testing/browser/testingCollectionService';
import { TestExplorerViewGrouping, TestExplorerViewMode, testStateNames } from 'vs/workbench/contrib/testing/common/constants';
import { AbstractIncrementalTestCollection, IncrementalChangeCollector, IncrementalTestCollectionItem, InternalTestItem, TestDiffOpType, TestIdWithProvider, TestsDiff } from 'vs/workbench/contrib/testing/common/testCollection';
import { TestingContextKeys } from 'vs/workbench/contrib/testing/common/testingContextKeys';
import { ITestService } from 'vs/workbench/contrib/testing/common/testService';
import { IEditorService } from 'vs/workbench/services/editor/common/editorService';
import { DebugAction, RunAction } from './testExplorerActions';

export class TestingExplorerView extends ViewPane {
	public viewModel!: TestingExplorerViewModel;
	private currentSubscription?: TestSubscriptionListener;
	private listContainer!: HTMLElement;
	private finishDiscovery?: () => void;

	constructor(
		options: IViewletViewOptions,
		@ITestingCollectionService private readonly testCollection: ITestingCollectionService,
		@ITestService private readonly testService: ITestService,
		@IProgressService private readonly progress: IProgressService,
		@IContextMenuService contextMenuService: IContextMenuService,
		@IKeybindingService keybindingService: IKeybindingService,
		@IConfigurationService configurationService: IConfigurationService,
		@IInstantiationService instantiationService: IInstantiationService,
		@IViewDescriptorService viewDescriptorService: IViewDescriptorService,
		@IContextKeyService contextKeyService: IContextKeyService,
		@IOpenerService openerService: IOpenerService,
		@IThemeService themeService: IThemeService,
		@ITelemetryService telemetryService: ITelemetryService,
	) {
		super(options, keybindingService, contextMenuService, configurationService, contextKeyService, viewDescriptorService, instantiationService, openerService, themeService, telemetryService);
		this._register(testService.onDidChangeProviders(() => this._onDidChangeViewWelcomeState.fire()));
	}

	/**
	 * @override
	 */
	public shouldShowWelcome() {
		return this.testService.providers === 0;
	}

	/**
	 * @override
	 */
	protected renderBody(container: HTMLElement): void {
		super.renderBody(container);

		this.listContainer = dom.append(container, dom.$('.test-explorer'));
		this.viewModel = this.instantiationService.createInstance(TestingExplorerViewModel, this.listContainer, this.onDidChangeBodyVisibility, this.currentSubscription);
		this._register(this.viewModel);

		this.updateProgressIndicator();
		this._register(this.testService.onBusyStateChange(t => {
			if (t.resource === ExtHostTestingResource.Workspace && t.busy !== (!!this.finishDiscovery)) {
				this.updateProgressIndicator();
			}
		}));

		this.getProgressIndicator().show(true);

		this._register(this.onDidChangeBodyVisibility(visible => {
			if (!visible && this.currentSubscription) {
				this.currentSubscription.dispose();
				this.currentSubscription = undefined;
				this.viewModel.replaceSubscription(undefined);
			} else if (visible && !this.currentSubscription) {
				this.currentSubscription = this.createSubscription();
				this.viewModel.replaceSubscription(this.currentSubscription);
			}
		}));
	}

	private updateProgressIndicator() {
		const busy = Iterable.some(this.testService.busyTestLocations, s => s.resource === ExtHostTestingResource.Workspace);
		if (!busy && this.finishDiscovery) {
			this.finishDiscovery();
			this.finishDiscovery = undefined;
		} else if (busy && !this.finishDiscovery) {
			const promise = new Promise<void>(resolve => { this.finishDiscovery = resolve; });
			this.progress.withProgress({ location: this.getProgressLocation() }, () => promise);
		}
	}

	/**
	 * @override
	 */
	protected layoutBody(height: number, width: number): void {
		super.layoutBody(height, width);
		this.listContainer.style.height = `${height}px`;
		this.viewModel.layout(height, width);
	}

	private createSubscription() {
		return this.testCollection.subscribeToWorkspaceTests();
	}
}

export class TestingExplorerViewModel extends Disposable {
	public tree: ObjectTree<ITestTreeElement, FuzzyScore>;
	private filter: TestsFilter;
	public projection!: ITestTreeProjection;

	private readonly _viewMode = TestingContextKeys.viewMode.bindTo(this.contextKeyService);
	private readonly _viewGrouping = TestingContextKeys.viewGrouping.bindTo(this.contextKeyService);

	/**
	 * Fires when the selected tests change.
	 */
	public readonly onDidChangeSelection: Event<ITreeEvent<ITestTreeElement | null>>;

	public get viewMode() {
		return this._viewMode.get() ?? TestExplorerViewMode.Tree;
	}

	public set viewMode(newMode: TestExplorerViewMode) {
		if (newMode === this._viewMode.get()) {
			return;
		}

		this._viewMode.set(newMode);
		this.updatePreferredProjection();
		this.storageService.store('testing.viewMode', newMode, StorageScope.WORKSPACE, StorageTarget.USER);
	}


	public get viewGrouping() {
		return this._viewGrouping.get() ?? TestExplorerViewGrouping.ByLocation;
	}

	public set viewGrouping(newGrouping: TestExplorerViewGrouping) {
		if (newGrouping === this._viewGrouping.get()) {
			return;
		}

		this._viewGrouping.set(newGrouping);
		this.updatePreferredProjection();
		this.storageService.store('testing.viewGrouping', newGrouping, StorageScope.WORKSPACE, StorageTarget.USER);
	}

	constructor(
		listContainer: HTMLElement,
		onDidChangeVisibility: Event<boolean>,
		private listener: TestSubscriptionListener | undefined,
		@IInstantiationService instantiationService: IInstantiationService,
		@IEditorService editorService: IEditorService,
		@ICodeEditorService codeEditorService: ICodeEditorService,
		@IStorageService private readonly storageService: IStorageService,
		@IContextKeyService private readonly contextKeyService: IContextKeyService,
	) {
		super();

		this._viewMode.set(this.storageService.get('testing.viewMode', StorageScope.WORKSPACE, TestExplorerViewMode.Tree) as TestExplorerViewMode);
		this._viewGrouping.set(this.storageService.get('testing.viewGrouping', StorageScope.WORKSPACE, TestExplorerViewGrouping.ByLocation) as TestExplorerViewGrouping);

		const labels = this._register(instantiationService.createInstance(ResourceLabels, { onDidChangeVisibility: onDidChangeVisibility }));

		this.filter = new TestsFilter();
		this.tree = instantiationService.createInstance(
			WorkbenchObjectTree,
			'Test Explorer List',
			listContainer,
			new ListDelegate(),
			[
				instantiationService.createInstance(TestsRenderer, labels)
			],
			{
				identityProvider: instantiationService.createInstance(IdentityProvider),
				hideTwistiesOfChildlessElements: true,
				sorter: instantiationService.createInstance(TreeSorter),
				keyboardNavigationLabelProvider: instantiationService.createInstance(TreeKeyboardNavigationLabelProvider),
				accessibilityProvider: instantiationService.createInstance(ListAccessibilityProvider),
				filter: this.filter,
			}) as ObjectTree<ITestTreeElement, FuzzyScore>;
		this._register(this.tree);

		this.updatePreferredProjection();

		this.onDidChangeSelection = this.tree.onDidChangeSelection;
		this._register(this.tree.onDidChangeSelection(evt => {
			const location = evt.elements[0]?.location;
			if (!location || !evt.browserEvent) {
				return;
			}

			editorService.openEditor({
				resource: location.uri,
				options: { selection: location.range, preserveFocus: true }
			});
		}));

		const tracker = this._register(new CodeEditorTracker(codeEditorService, this));
		this._register(onDidChangeVisibility(visible => {
			if (visible) {
				tracker.activate();
			} else {
				tracker.deactivate();
			}
		}));
	}

	/**
	 * Re-layout the tree.
	 */
	public layout(height: number, width: number): void {
		this.tree.layout(height, width);
	}

	/**
	 * Replaces the test listener and recalculates the tree.
	 */
	public replaceSubscription(listener: TestSubscriptionListener | undefined) {
		this.listener = listener;
		this.updatePreferredProjection();
	}

	/**
	 * Reveals and moves focus to the item.
	 */
	public async revealItem(item: ITestTreeElement, reveal = true): Promise<void> {
		const chain: ITestTreeElement[] = [];
		for (let parent = item.parentItem; parent; parent = parent.parentItem) {
			chain.push(parent);
		}

		for (const parent of chain.reverse()) {
			try {
				this.tree.expand(parent);
			} catch {
				// ignore if not present
			}
		}

		if (reveal === true && this.tree.getRelativeTop(item) === null) {
			// Don't scroll to the item if it's already visible, or if set not to.
			this.tree.reveal(item, 0.5);
		}

		this.tree.setFocus([item]);
		this.tree.setSelection([item]);
	}

	private updatePreferredProjection() {
		this.projection?.dispose();
		if (!this.listener) {
			this.tree.setChildren(null, []);
			return;
		}

		if (this._viewGrouping.get() === TestExplorerViewGrouping.ByLocation) {
			if (this._viewMode.get() === TestExplorerViewMode.List) {
				this.projection = new ListProjection(this.listener);
			} else {
				this.projection = new HierarchalProjection(this.listener);
			}
		} else {
			if (this._viewMode.get() === TestExplorerViewMode.List) {
				this.projection = new StatusListProjection(this.listener);
			} else {
				this.projection = new StatusProjection(this.listener);
			}
		}

		this.projection.onUpdate(this.deferUpdate, this);
		this.projection.applyTo(this.tree);
	}

	@throttle(200)
	private deferUpdate() {
		this.projection.applyTo(this.tree);
	}

	/**
	 * Gets the selected tests from the tree.
	 */
	public getSelectedTests() {
		return this.tree.getSelection();
	}
}

class CodeEditorTracker {
	private store = new DisposableStore();
	private lastRevealed?: ITestTreeElement;

	constructor(@ICodeEditorService private readonly codeEditorService: ICodeEditorService, private readonly model: TestingExplorerViewModel) {
	}

	public activate() {
		const editorStores = new Set<DisposableStore>();
		this.store.add(toDisposable(() => {
			for (const store of editorStores) {
				store.dispose();
			}
		}));

		const register = (editor: ICodeEditor) => {
			const store = new DisposableStore();
			const uri = editor.getModel()?.uri;
			if (!uri) {
				return;
			}

			store.add(editor.onDidChangeCursorPosition(evt => {
				const test = this.model.projection.getTestAtPosition(uri, evt.position);

				if (test && test !== this.lastRevealed) {
					this.model.revealItem(test);
					this.lastRevealed = test;
				}
			}));

			editor.onDidDispose(() => {
				store.dispose();
				editorStores.delete(store);
			});
		};

		this.store.add(this.codeEditorService.onCodeEditorAdd(register));
		this.codeEditorService.listCodeEditors().forEach(register);
	}

	public deactivate() {
		this.store.dispose();
		this.store = new DisposableStore();
	}

	public dispose() {
		this.store.dispose();
	}
}

/**
 * Gets the computed state for the node.
 */
const getComputedState = (node: ITestTreeElement) => {
	if (node.computedState === undefined) {
		node.computedState = node.state ?? TestRunState.Unset;
		for (const child of node.getChildren()) {
			node.computedState = maxPriority(node.computedState, getComputedState(child));
		}
	}

	return node.computedState;
};

/**
 * Refreshes the computed state for the node and its parents. Any changes
 * elements will be added to the `changedNodes` set.
 */
const refreshComputedState = (node: ITestTreeElement, addUpdated: (n: ITestTreeElement) => void) => {
	if (node.computedState === undefined) {
		return;
	}

	const oldPriority = statePriority[node.computedState];
	node.computedState = undefined;
	const newState = getComputedState(node);
	const newPriority = statePriority[getComputedState(node)];
	if (newPriority === oldPriority) {
		return;
	}

	addUpdated(node);
	if (newPriority > oldPriority) {
		// Update all parents to ensure they're at least this priority.
		for (let parent = node.parentItem; parent; parent = parent.parentItem) {
			const prev = parent.computedState;
			if (prev !== undefined && statePriority[prev] >= newPriority) {
				break;
			}

			parent.computedState = newState;
			addUpdated(parent);
		}
	} else if (newPriority < oldPriority) {
		// Re-render all parents of this node whose computed priority might have come from this node
		for (let parent = node.parentItem; parent; parent = parent.parentItem) {
			const prev = parent.computedState;
			if (prev === undefined || statePriority[prev] > oldPriority) {
				break;
			}

			parent.computedState = undefined;
			parent.computedState = getComputedState(parent);
			addUpdated(parent);
		}
	}
};

class TestsFilter implements ITreeFilter<ITestTreeElement, FuzzyScore> {
	private filterText: string | undefined;

	public setFilter(filterText: string) {
		this.filterText = filterText;
	}

	public filter(element: ITestTreeElement): TreeFilterResult<FuzzyScore> {
		if (element instanceof ListElement && element.elementType !== ListElementType.TestLeaf && !element.isTestRoot) {
			return TreeVisibility.Hidden;
		}

		if (element instanceof ListStateElement && element.elementType !== ListElementType.TestLeaf) {
			return TreeVisibility.Hidden;
		}

		if (!this.filterText) {
			return TreeVisibility.Visible;
		}

		if (element.label.includes(this.filterText)) {
			return TreeVisibility.Visible;
		}

		return TreeVisibility.Recurse;
	}
}
class TreeSorter implements ITreeSorter<ITestTreeElement> {
	public compare(a: ITestTreeElement, b: ITestTreeElement): number {
		if (a instanceof StateElement && b instanceof StateElement) {
			return cmpPriority(a.computedState, b.computedState);
		}

		return a.label.localeCompare(b.label);
	}
}

class ListAccessibilityProvider implements IListAccessibilityProvider<ITestTreeElement> {
	getWidgetAriaLabel(): string {
		return localize('testExplorer', "Test Explorer");
	}

	getAriaLabel(element: ITestTreeElement): string {
		return element.label;
	}
}

class TreeKeyboardNavigationLabelProvider implements IKeyboardNavigationLabelProvider<ITestTreeElement> {
	getKeyboardNavigationLabel(element: ITestTreeElement) {
		return element.label;
	}
}

class ListDelegate implements IListVirtualDelegate<ITestTreeElement> {
	getHeight(_element: ITestTreeElement) {
		return 22;
	}

	getTemplateId(_element: ITestTreeElement) {
		return TestsRenderer.ID;
	}
}

class IdentityProvider implements IIdentityProvider<ITestTreeElement> {
	public getId(element: ITestTreeElement) {
		return element.treeId;
	}
}

interface TestTemplateData {
	label: IResourceLabel;
	icon: HTMLElement;
	actionBar: ActionBar;
}

class TestsRenderer implements ITreeRenderer<ITestTreeElement, FuzzyScore, TestTemplateData> {
	public static readonly ID = 'testExplorer';

	constructor(
		private labels: ResourceLabels,
		@IInstantiationService private readonly instantiationService: IInstantiationService
	) { }

	get templateId(): string {
		return TestsRenderer.ID;
	}

	public renderTemplate(container: HTMLElement): TestTemplateData {
		const wrapper = dom.append(container, dom.$('.test-item'));

		const icon = dom.append(wrapper, dom.$('.computed-state'));
		const name = dom.append(wrapper, dom.$('.name'));
		const label = this.labels.create(name, { supportHighlights: true });

		const actionBar = new ActionBar(wrapper, {
			actionViewItemProvider: action =>
				action instanceof MenuItemAction
					? this.instantiationService.createInstance(MenuEntryActionViewItem, action)
					: undefined
		});

		return { label, actionBar, icon };
	}

	public renderElement(node: ITreeNode<ITestTreeElement, FuzzyScore>, index: number, data: TestTemplateData): void {
		const element = node.element;
		const label: IResourceLabelProps = { name: element.label };
		const options: IResourceLabelOptions = {};
		data.actionBar.clear();

		const state = getComputedState(element);
		const icon = testingStatesToIcons.get(state);
		data.icon.className = 'computed-state ' + (icon ? ThemeIcon.asClassName(icon) : '');
		if (state === TestRunState.Running) {
			data.icon.className += ' codicon-modifier-spin';
		}

		const test = element.test;
		if (test) {
			if (test.item.location) {
				label.resource = test.item.location.uri;
			}

			options.title = 'hover title';
			options.fileKind = FileKind.FILE;

			label.description = element.description;
		} else {
			options.fileKind = FileKind.ROOT_FOLDER;
		}

		const running = state === TestRunState.Running;
		if (!Iterable.isEmpty(element.runnable)) {
			data.actionBar.push(
				this.instantiationService.createInstance(RunAction, element.runnable, running),
				{ icon: true, label: false },
			);
		}

		if (!Iterable.isEmpty(element.debuggable)) {
			data.actionBar.push(
				this.instantiationService.createInstance(DebugAction, element.debuggable, running),
				{ icon: true, label: false },
			);
		}

		data.label.setResource(label, options);
	}

	disposeTemplate(templateData: TestTemplateData): void {
		templateData.label.dispose();
		templateData.actionBar.dispose();
	}
}


export interface ITestTreeProjection extends IDisposable {
	/**
	 * Event that fires when the projection changes.
	 */
	onUpdate: Event<void>;

	/**
	 * Gets the test at the given position in th editor. Should be fast,
	 * since it is called on each cursor move.
	 */
	getTestAtPosition(uri: URI, position: Position): ITestTreeElement | undefined;

	/**
	 * Applies pending update to the tree.
	 */
	applyTo(tree: ObjectTree<ITestTreeElement, FuzzyScore>): void;
}

export interface ITestTreeElement {
	/**
	 * Computed element state. Will be set automatically if not initially provided.
	 * The projection is responsible for clearing (or updating) this if it
	 * becomes invalid.
	 */
	computedState: TestRunState | undefined;

	/**
	 * Unique ID of the element in the tree.
	 */
	readonly treeId: string;

	/**
	 * Location of the test, if any.
	 */
	readonly location?: { uri: URI; range: ITextEditorSelection };

	/**
	 * Test item, if any.
	 */
	readonly test?: Readonly<InternalTestItem>;

	/**
	 * Tree description.
	 */
	readonly description?: string;

	/**
	 * Depth of the item in the tree.
	 */
	readonly depth: number;

	/**
	 * Tests that can be run using this tree item.
	 */
	readonly runnable: Iterable<TestIdWithProvider>;

	/**
	 * Tests that can be run using this tree item.
	 */
	readonly debuggable: Iterable<TestIdWithProvider>;

	/**
	 * State of of the tree item. Mostly used for deriving the computed state.
	 */
	readonly state?: TestRunState;
	readonly label: string;
	readonly parentItem: ITestTreeElement | null;
	getChildren(): Iterable<ITestTreeElement>;
}

class HierarchalElement implements ITestTreeElement {
	public readonly children = new Set<HierarchalElement>();
	public computedState: TestRunState | undefined;
	public readonly depth: number = this.parentItem.depth + 1;

	public get treeId() {
		return `test:${this.test.id}`;
	}

	public get label() {
		return this.test.item.label;
	}

	public get state() {
		return this.test.item.state.runState;
	}

	public get location() {
		return this.test.item.location;
	}

	public get runnable() {
		return this.test.item.runnable ? [this.test.item] : Iterable.empty();
	}

	public get debuggable() {
		return this.test.item.debuggable ? [this.test.item] : Iterable.empty();
	}

	constructor(public readonly test: InternalTestItem, public readonly parentItem: HierarchalFolder | HierarchalElement) {
		this.test = { ...test, item: { ...test.item } }; // clone since we Object.assign updatese
	}

	public getChildren() {
		return this.children;
	}

	public update(actual: InternalTestItem, addUpdated: (n: ITestTreeElement) => void) {
		const stateChange = actual.item.state.runState !== this.state;
		Object.assign(this.test, actual);
		if (stateChange) {
			refreshComputedState(this, addUpdated);
		}
	}
}

class HierarchalFolder implements ITestTreeElement {
	public readonly children = new Set<HierarchalElement>();
	public readonly parentItem = null;
	public readonly depth = 0;
	public computedState: TestRunState | undefined;

	public get treeId() {
		return `folder:${this.folder.index}`;
	}

	public get runnable() {
		return Iterable.concatNested(Iterable.map(this.children, c => c.runnable));
	}

	public get debuggable() {
		return Iterable.concatNested(Iterable.map(this.children, c => c.debuggable));
	}

	constructor(private readonly folder: IWorkspaceFolder) { }

	public get label() {
		return this.folder.name;
	}

	public getChildren() {
		return this.children;
	}
}

const enum ListElementType {
	TestLeaf,
	BranchWithLeaf,
	BranchWithoutLeaf,
	Unset,
}

class ListElement extends HierarchalElement {
	public elementType: ListElementType = ListElementType.Unset;
	public readonly isTestRoot = !this.actualParent;
	private readonly actualChildren = new Set<ListElement>();

	public get description() {
		let description: string | undefined;
		for (let parent = this.actualParent; parent && !parent.isTestRoot; parent = parent.actualParent) {
			description = description ? `${parent.label} › ${description}` : parent.label;
		}

		return description;
	}

	/**
	 * @param actualParent Parent of the item in the test heirarchy
	 */
	constructor(
		internal: InternalTestItem,
		parentItem: HierarchalFolder | HierarchalElement,
		private readonly addUpdated: (n: ITestTreeElement) => void,
		private readonly actualParent?: ListElement,
	) {
		super(internal, parentItem);
		actualParent?.addChild(this);
		this.updateLeafTestState();
	}

	/**
	 * @override
	 */
	public update(actual: InternalTestItem, addUpdated: (n: ITestTreeElement) => void) {
		const wasRunnable = this.test.item.runnable;
		super.update(actual, addUpdated);

		if (this.test.item.runnable !== wasRunnable) {
			this.updateLeafTestState();
		}
	}

	/**
	 * Should be called when the list element is removed.
	 */
	public remove() {
		this.actualParent?.removeChild(this);
	}

	private removeChild(element: ListElement) {
		this.actualChildren.delete(element);
		this.updateLeafTestState();
	}

	private addChild(element: ListElement) {
		this.actualChildren.add(element);
		this.updateLeafTestState();
	}

	/**
	 * Updates the test leaf state for this node. Should be called when a child
	 * or this node is modified. Note that we never need to look at the children
	 * here, the children will already be leaves, or not.
	 */
	private updateLeafTestState() {
		const newType = Iterable.some(this.actualChildren, c => c.elementType !== ListElementType.BranchWithoutLeaf)
			? ListElementType.BranchWithLeaf
			: this.test.item.runnable
				? ListElementType.TestLeaf
				: ListElementType.BranchWithoutLeaf;

		if (newType !== this.elementType) {
			this.elementType = newType;
			this.addUpdated(this);
		}

		this.actualParent?.updateLeafTestState();
	}
}

/**
 * Stores and looks up items by their uri/range.
 */
class TestLocationStore<T extends { location?: ModeLocation, depth: number }> {
	private readonly itemsByUri = new Map<string, T[]>();

	public getTestAtPosition(uri: URI, position: Position) {
		const tests = this.itemsByUri.get(uri.toString());
		if (!tests) {
			return;
		}

		return tests.find(test => {
			const range = test.location?.range;
			return range
				&& new Position(range.startLineNumber, range.startColumn).isBeforeOrEqual(position)
				&& position.isBefore(new Position(
					range.endLineNumber ?? range.startLineNumber,
					range.endColumn ?? range.startColumn,
				));
		});
	}

	public remove(item: T, fromLocation = item.location) {
		if (!fromLocation) {
			return;
		}

		const key = fromLocation.uri.toString();
		const arr = this.itemsByUri.get(key);
		if (!arr) {
			return;
		}

		for (let i = 0; i < arr.length; i++) {
			if (arr[i] === item) {
				arr.splice(i, 1);
				return;
			}
		}
	}

	public add(item: T) {
		if (!item.location) {
			return;
		}

		const key = item.location.uri.toString();
		const arr = this.itemsByUri.get(key);
		if (!arr) {
			this.itemsByUri.set(key, [item]);
			return;
		}

		arr.splice(findFirstInSorted(arr, x => x.depth < item.depth), 0, item);
	}
}

const locationsEqual = (a: ModeLocation | undefined, b: ModeLocation | undefined) => {
	if (a === undefined || b === undefined) {
		return b === a;
	}

	return a.uri.toString() === b.uri.toString()
		&& a.range.startLineNumber === b.range.startLineNumber
		&& a.range.startColumn === b.range.startColumn
		&& a.range.endLineNumber === b.range.endLineNumber
		&& a.range.endColumn === b.range.endColumn;
};

/**
 * Projection that lists tests in their traditional tree view.
 */
class HierarchalProjection extends Disposable implements ITestTreeProjection {
	private readonly updateEmitter = new Emitter<void>();
	private lastHadMultipleFolders = true;
	private newlyRenderedNodes = new Set<HierarchalElement | HierarchalFolder>();
	private updatedNodes = new Set<HierarchalElement | HierarchalFolder>();
	private removedNodes = new Set<HierarchalElement | HierarchalFolder>();
	private readonly locations = new TestLocationStore<HierarchalElement>();

	/**
	 * Map of item IDs to test item objects.
	 */
	protected readonly items = new Map<string, HierarchalElement>();

	/**
	 * Root folders
	 */
	protected readonly folders = new Map<string, HierarchalFolder>();

	/**
	 * @inheritdoc
	 */
	public readonly onUpdate = this.updateEmitter.event;

	constructor(listener: TestSubscriptionListener) {
		super();
		this._register(listener.onDiff(([folder, diff]) => this.applyDiff(folder, diff)));
		this._register(listener.onFolderChange(this.applyFolderChange, this));

		for (const [folder, collection] of listener.workspaceFolderCollections) {
			const queue = [collection.rootNodes];
			while (queue.length) {
				for (const id of queue.pop()!) {
					const node = collection.getNodeById(id)!;
					const item = this.createItem(node, folder.folder);
					this.storeItem(item);
					queue.push(node.children);
				}
			}
		}

		for (const folder of this.folders.values()) {
			this.newlyRenderedNodes.add(folder);
		}
	}

	private applyFolderChange(evt: IWorkspaceFoldersChangeEvent) {
		for (const folder of evt.removed) {
			const existing = this.folders.get(folder.uri.toString());
			if (existing) {
				this.folders.delete(folder.uri.toString());
				this.removedNodes.add(existing);
			}
			this.updateEmitter.fire();
		}
	}

	/**
	 * @inheritdoc
	 */
	public getTestAtPosition(uri: URI, position: Position) {
		return this.locations.getTestAtPosition(uri, position);
	}

	/**
	 * @inheritdoc
	 */
	private applyDiff(folder: IWorkspaceFolder, diff: TestsDiff) {
		for (const op of diff) {
			switch (op[0]) {
				case TestDiffOpType.Add: {
					const item = this.createItem(op[1], folder);
					this.storeItem(item);
					this.newlyRenderedNodes.add(item);
					break;
				}

				case TestDiffOpType.Update: {
					const item = op[1];
					const existing = this.items.get(item.id);
					if (!existing) {
						break;
					}

					const locationChanged = !locationsEqual(existing.location, item.item.location);
					if (locationChanged) { this.locations.remove(existing); }
					existing.update(item, this.addUpdated);
					if (locationChanged) { this.locations.add(existing); }
					this.addUpdated(existing);
					break;
				}

				case TestDiffOpType.Remove: {
					const toRemove = this.items.get(op[1]);
					if (!toRemove) {
						break;
					}

					this.deleteItem(toRemove);
					toRemove.parentItem.children.delete(toRemove);
					this.removedNodes.add(toRemove);

					const queue: Iterable<HierarchalElement>[] = [[toRemove]];
					while (queue.length) {
						for (const item of queue.pop()!) {
							this.unstoreItem(item);
							this.newlyRenderedNodes.delete(item);
						}
					}
				}
			}
		}

		for (const [key, folder] of this.folders) {
			if (folder.children.size === 0) {
				this.removedNodes.add(folder);
				this.folders.delete(key);
			}
		}

		if (diff.length !== 0) {
			this.updateEmitter.fire();
		}
	}

	/**
	 * @inheritdoc
	 */
	public applyTo(tree: ObjectTree<ITestTreeElement, FuzzyScore>) {
		const firstFolder = Iterable.first(this.folders.values());

		if (!this.lastHadMultipleFolders && this.folders.size !== 1) {
			tree.setChildren(null, Iterable.map(this.folders.values(), this.renderNode));
			this.lastHadMultipleFolders = true;
		} else if (this.lastHadMultipleFolders && this.folders.size === 1) {
			tree.setChildren(null, Iterable.map(firstFolder!.children, this.renderNode));
			this.lastHadMultipleFolders = false;
		} else {
			const alreadyUpdatedChildren = new Set<HierarchalElement | HierarchalFolder | null>();
			for (const nodeList of [this.newlyRenderedNodes, this.removedNodes]) {
				for (let { parentItem, children } of nodeList) {
					if (!alreadyUpdatedChildren.has(parentItem)) {
						if (!this.lastHadMultipleFolders && parentItem === firstFolder) {
							tree.setChildren(null, Iterable.map(firstFolder.children, this.renderNode));
						} else {
							const pchildren: Iterable<HierarchalElement | HierarchalFolder> = parentItem?.children ?? this.folders.values();
							tree.setChildren(parentItem, Iterable.map(pchildren, this.renderNode));
						}

						alreadyUpdatedChildren.add(parentItem);
					}

					for (const child of children) {
						alreadyUpdatedChildren.add(child);
					}
				}
			}

			if (!this.lastHadMultipleFolders) {
				this.updatedNodes.delete(firstFolder!);
			}

			for (const node of this.updatedNodes) {
				if (!alreadyUpdatedChildren.has(node)) {
					tree.rerender(node);
				}
			}
		}

		this.newlyRenderedNodes.clear();
		this.removedNodes.clear();
		this.updatedNodes.clear();
	}

	protected createItem(item: InternalTestItem, folder: IWorkspaceFolder): HierarchalElement {
		const parent = item.parent ? this.items.get(item.parent)! : this.getOrCreateFolderElement(folder);
		return new HierarchalElement(item, parent);
	}

	protected deleteItem(item: HierarchalElement) {
		// no-op
	}

	protected getOrCreateFolderElement(folder: IWorkspaceFolder) {
		let f = this.folders.get(folder.uri.toString());
		if (!f) {
			f = new HierarchalFolder(folder);
			this.newlyRenderedNodes.add(f);
			this.folders.set(folder.uri.toString(), f);
		}

		return f;
	}

	protected readonly addUpdated = (item: ITestTreeElement) => {
		const cast = item as HierarchalElement | HierarchalFolder;
		if (!this.newlyRenderedNodes.has(cast)) {
			this.updatedNodes.add(cast);
		}
	};

	private readonly renderNode = (node: HierarchalElement | HierarchalFolder): ITreeElement<ITestTreeElement> => {
		return {
			element: node,
			children: Iterable.map(node.children, this.renderNode),
		};
	};

	private unstoreItem(item: HierarchalElement) {
		this.items.delete(item.test.id);
		this.locations.add(item);
	}

	protected storeItem(item: HierarchalElement) {
		item.parentItem.children.add(item);
		this.items.set(item.test.id, item);
		this.locations.remove(item);
	}
}

/**
 * Projection that shows tests in a flat list (grouped by provider). The only
 * change is that, while creating the item, the item parent is set to the
 * test root rather than the heirarchal parent.
 */
class ListProjection extends HierarchalProjection {
	/**
	 * @override
	 */
	protected createItem(item: InternalTestItem, folder: IWorkspaceFolder): HierarchalElement {
		const parent = this.getOrCreateFolderElement(folder);
		const actualParent = item.parent ? this.items.get(item.parent) as ListElement : undefined;
		for (const testRoot of parent.children) {
			if (testRoot.test.providerId === item.providerId) {
				return new ListElement(item, testRoot, this.addUpdated, actualParent);
			}
		}

		return new ListElement(item, parent, this.addUpdated);
	}

	/**
	 * @override
	 */
	protected deleteItem(item: HierarchalElement) {
		(item as ListElement).remove();
	}
}

class StateElement implements ITestTreeElement {
	public computedState = this.state;

	public get treeId() {
		return `state:${this.state}`;
	}

	public readonly depth = 0;
	public readonly label = testStateNames[this.state];
	public readonly parentItem = null;
	public readonly children = new Set<TestStateElement>();

	getChildren(): Iterable<ITestTreeElement> {
		return this.children;
	}

	public get runnable() {
		return Iterable.concatNested(Iterable.map(this.children, c => c.runnable));
	}

	public get debuggable() {
		return Iterable.concatNested(Iterable.map(this.children, c => c.debuggable));
	}

	constructor(public readonly state: TestRunState) { }
}

class TestStateElement implements ITestTreeElement {
	public computedState = this.state;

	public get treeId() {
		return `test:${this.test.id}`;
	}

	public get label() {
		return this.test.item.label;
	}

	public get location() {
		return this.test.item.location;
	}

	public get runnable(): Iterable<TestIdWithProvider> {
		// if this item is runnable and all its children are in the same state,
		// we can run all of them in one go. This will eventually be true
		// for leaf nodes, whose treeElements contain only their own state.
		if (this.test.item.runnable && this.test.treeElements.size === 1) {
			return [{ testId: this.test.id, providerId: this.test.providerId }];
		}

		return Iterable.concatNested(Iterable.map(this.children, c => c.runnable));
	}

	public get debuggable(): Iterable<TestIdWithProvider> {
		// same logic as runnable above
		if (this.test.item.debuggable && this.test.treeElements.size === 1) {
			return [{ testId: this.test.id, providerId: this.test.providerId }];
		}

		return Iterable.concatNested(Iterable.map(this.children, c => c.debuggable));
	}

	public readonly depth = 0;
	public readonly children = new Set<TestStateElement>();

	getChildren(): Iterable<ITestTreeElement> {
		return this.children;
	}

	constructor(
		public readonly state: TestRunState,
		public readonly test: IStatusTestItem,
		public readonly parentItem: TestStateElement | StateElement,
	) {
		parentItem.children.add(this);
	}

	public remove() {
		this.parentItem.children.delete(this);
	}
}

class ListStateElement extends TestStateElement {
	public elementType: ListElementType = ListElementType.Unset;
	public readonly children = new Set<ListStateElement>();

	constructor(
		state: TestRunState,
		test: IStatusTestItem,
		parentItem: StateElement,
		private readonly addUpdated: (n: ListStateElement) => void,
		private readonly actualParent?: ListStateElement,
	) {
		super(state, test, parentItem);
		actualParent?.addChild(this);
		this.updateLeafTestState();
	}

	/**
	 * Should be called when the list element is removed.
	 */
	public remove() {
		super.remove();
		this.actualParent?.removeChild(this);
	}

	public removeChild(element: ListStateElement) {
		this.children.delete(element);
		this.updateLeafTestState();
	}

	public addChild(element: ListStateElement) {
		this.children.add(element);
		this.updateLeafTestState();
	}

	/**
	 * Updates the test leaf state for this node. Should be called when a child
	 * or this node is modified. Note that we never need to look at the children
	 * here, the children will already be leaves, or not.
	 */
	private updateLeafTestState() {
		const oldType = this.elementType;
		const newType = Iterable.some(this.children, c => oldType !== ListElementType.BranchWithoutLeaf)
			? ListElementType.BranchWithLeaf
			: this.test.item.runnable
				? ListElementType.TestLeaf
				: ListElementType.BranchWithoutLeaf;

		if (newType !== this.elementType) {
			this.elementType = newType;
			if (oldType !== ListElementType.Unset) {
				this.addUpdated(this);
			}
		}

		this.actualParent?.updateLeafTestState();
	}
}

interface IStatusTestItem extends IncrementalTestCollectionItem {
	treeElements: Map<TestRunState, TestStateElement>;
	previousState: TestRunState;
	depth: number;
	parentItem?: IStatusTestItem;
	location?: ModeLocation;
}

class StatusProjection extends AbstractIncrementalTestCollection<IStatusTestItem> implements ITestTreeProjection {
	private readonly updateEmitter = new Emitter<void>();
	protected newlyRenderedNodes = new Set<StateElement | TestStateElement>();
	private updatedNodes = new Set<StateElement | TestStateElement>();
	protected removedNodes = new Set<StateElement | TestStateElement>();
	private readonly locations = new TestLocationStore<IStatusTestItem>();
	private readonly disposable = new DisposableStore();

	/**
	 * @inheritdoc
	 */
	public readonly onUpdate = this.updateEmitter.event;

	/**
	 * Root elements for states in the tree.
	 */
	protected readonly stateRoots = new Map<TestRunState, StateElement>();

	constructor(listener: TestSubscriptionListener) {
		super();

		this.disposable.add(listener.onDiff(([, diff]) => this.apply(diff)));

		const firstDiff: TestsDiff = [];
		for (const [, collection] of listener.workspaceFolderCollections) {
			const queue = [collection.rootNodes];
			while (queue.length) {
				for (const id of queue.pop()!) {
					const node = collection.getNodeById(id)!;
					firstDiff.push([TestDiffOpType.Add, node]);
					queue.push(node.children);
				}
			}
		}

		this.apply(firstDiff);
	}

	/**
	 * Frees listeners associated with the projection.
	 */
	public dispose() {
		this.disposable.dispose();
	}

	/**
	 * @inheritdoc
	 */
	public getTestAtPosition(uri: URI, position: Position) {
		const item = this.locations.getTestAtPosition(uri, position);
		if (!item) {
			return undefined;
		}

		for (const state of statesInOrder) {
			const element = item.treeElements.get(state);
			if (element) {
				return element;
			}
		}

		return undefined;
	}

	/**
	 * @inheritdoc
	 */
	public applyTo(tree: ObjectTree<ITestTreeElement, FuzzyScore>) {
		const alreadyUpdatedChildren = new Set<StateElement | TestStateElement | null>();
		for (const nodeList of [this.newlyRenderedNodes, this.removedNodes]) {
			for (let { parentItem, children } of nodeList) {
				if (!alreadyUpdatedChildren.has(parentItem)) {
					const pchildren: Iterable<StateElement | TestStateElement> = parentItem?.children ?? this.stateRoots.values();
					tree.setChildren(parentItem, Iterable.map(pchildren, this.renderNode));

					alreadyUpdatedChildren.add(parentItem);
				}

				for (const child of children) {
					alreadyUpdatedChildren.add(child);
				}
			}
		}

		for (const node of this.updatedNodes) {
			if (!alreadyUpdatedChildren.has(node)) {
				tree.rerender(node);
			}
		}

		this.newlyRenderedNodes.clear();
		this.removedNodes.clear();
		this.updatedNodes.clear();
	}

	protected readonly addUpdated = (node: StateElement | TestStateElement) => {
		if (!this.newlyRenderedNodes.has(node)) {
			this.updatedNodes.add(node);
		}
	};

	private readonly renderNode = (node: StateElement | TestStateElement): ITreeElement<ITestTreeElement> => {
		return {
			element: node,
			children: Iterable.map(node.children, this.renderNode),
		};
	};

	/**
	 * @override
	 */
	protected createChangeCollector(): IncrementalChangeCollector<IStatusTestItem> {
		return {
			add: node => {
				this.resolveNodesRecursive(node);
				this.locations.add(node);
			},
			remove: (node, isNested) => {
				this.locations.remove(node);

				if (!isNested) {
					for (const state of node.treeElements.keys()) {
						this.pruneStateElements(node, state, true);
					}
				}
			},
			update: node => {
				if (node.item.state.runState !== node.previousState) {
					this.pruneStateElements(node, node.previousState);
					this.resolveNodesRecursive(node);
				}

				const locationChanged = !locationsEqual(node.location, node.item.location);
				if (locationChanged) {
					this.locations.remove(node);
					node.location = node.item.location;
					this.locations.add(node);
				}

				this.addUpdated(node.treeElements.get(node.item.state.runState)!);
			},
			complete: () => {
				this.updateEmitter.fire();
			}
		};
	}

	/**
	 * Ensures tree nodes for the item state are present in the tree.
	 */
	protected resolveNodesRecursive(item: IStatusTestItem) {
		const state = item.item.state.runState;
		item.previousState = item.item.state.runState;

		// Create a list of items until the current item who don't have a tree node for the status yet
		let chain: IStatusTestItem[] = [];
		for (let i: IStatusTestItem | undefined = item; i && !i.treeElements.has(state); i = i.parentItem) {
			chain.push(i);
		}

		for (let i = chain.length - 1; i >= 0; i--) {
			const item2 = chain[i];
			// the loop would have stopped pushing parents when either it reaches
			// the root, or it reaches a parent who already has a node for this state.
			const parent = item2.parentItem?.treeElements.get(state) ?? this.getOrCreateStateElement(state);
			const node = this.createElement(state, item2, parent);

			item2.treeElements.set(state, node);
			parent.children.add(node);

			if (i === chain.length - 1) {
				this.newlyRenderedNodes.add(node);
			}
		}
	}

	protected createElement(state: TestRunState, item: IStatusTestItem, parent: TestStateElement | StateElement) {
		return new TestStateElement(state, item, parent);
	}


	/**
	 * Recursively (from the leaf to the root) removes tree elements if there's
	 * no children who have the given state left.
	 *
	 * Returns true if it resulted in a node being removed.
	 */
	protected pruneStateElements(item: IStatusTestItem | undefined, state: TestRunState, force = false) {
		if (!item) {
			const stateRoot = this.stateRoots.get(state);
			if (stateRoot?.children.size === 0) {
				this.removedNodes.add(stateRoot);
				this.stateRoots.delete(state);
				return true;
			}

			return false;
		}

		const node = item.treeElements.get(state);
		if (!node) {
			return false;
		}

		// Check to make sure we aren't in the state, and there's no child with the state...
		if (!force) {
			if (item.item.state.runState === state) {
				return false;
			}

			for (const childId of item.children) {
				if (this.items.get(childId)?.treeElements.has(state)) {
					return false;
				}
			}
		}

		// If so, proceed to deletion and recurse upwards.
		item.treeElements.delete(state);
		node.remove();

		if (!this.pruneStateElements(item.parentItem, state)) {
			this.removedNodes.add(node);
		}

		return true;
	}

	protected getOrCreateStateElement(state: TestRunState) {
		let s = this.stateRoots.get(state);
		if (!s) {
			s = new StateElement(state);
			this.newlyRenderedNodes.add(s);
			this.stateRoots.set(state, s);
		}

		return s;
	}

	protected createItem(item: InternalTestItem, parentItem?: IStatusTestItem): IStatusTestItem {
		return {
			...item,
			depth: parentItem ? parentItem.depth + 1 : 0,
			parentItem: parentItem,
			previousState: item.item.state.runState,
			location: item.item.location,
			children: new Set(),
			treeElements: new Map(),
		};
	}
}

/**
 * Projection that shows tests in a flat list (grouped by status). The only
 * change is that, while creating the item, the item parent is set to the
 * state root.
 *
 * Note that if we didn't have the status projection, the implementation of
 * this alone would be a lot simpler. But we may as well reuse it here.
 */
class StatusListProjection extends StatusProjection {
	/**
	 * @override
	 */
	protected createElement(state: TestRunState, item: IStatusTestItem, parent: TestStateElement | StateElement) {
		return new ListStateElement(state,
			item,
			this.getOrCreateStateElement(state),
			this.addUpdated,
			parent instanceof ListStateElement ? parent : undefined,
		);
	}
}
